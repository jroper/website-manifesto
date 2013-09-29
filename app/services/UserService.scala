package services

import play.modules.reactivemongo.ReactiveMongoPlugin
import play.modules.reactivemongo.json.collection.JSONCollection
import play.modules.reactivemongo.json.BSONFormats._
import play.api.Play._
import models._
import reactivemongo.bson.{BSONDateTime, BSONObjectID}
import reactivemongo.core.commands.GetLastError
import scala.concurrent.Future
import play.api.libs.concurrent.Execution.Implicits._
import org.joda.time.DateTime
import play.api.libs.json._

object UserService {

  // Must use this format for MongoDB
  implicit def signatoryFormat = Signatory.dbSignatoryFormat

  /**
   * A user that has logged in using OAuth.
   *
   * @param provider The service specific identifier for the user.
   * @param name The name of the user.
   * @param avatar The users avatar, if they have one.
   * @param signed When the user signed the manifesto, if they signed it.
   * @param version The version they signed, if they signed it.
   */
  case class OAuthUser(provider: Provider, name: String, avatar: Option[String], signed: Option[DateTime] = None,
                       version: Option[Version] = None)

  lazy val collection = ReactiveMongoPlugin.db.collection[JSONCollection]("signatories")

  /**
   * Find the given OAuth user, and if the user can't be found, create a new one.
   *
   * @param user The user to save
   * @return A future of the found or saved signatory
   */
  def findOrSaveUser(user: OAuthUser): Future[Signatory] = {
    def providerAndId: (String, JsValue) = user.provider match {
      case Twitter(id, _) => ("twitter", JsNumber(id))
      case Google(id) => ("google", JsString(id))
      case GitHub(id, _) => ("github", JsNumber(id))
      case LinkedIn(id) => ("linkedin", JsString(id))
    }
    def find = collection.find(Json.obj(
      "provider.id" -> providerAndId._1,
      "provider.details.id" -> providerAndId._2
    )).cursor[Signatory].headOption()

    def returnOrSave(s: Option[Signatory]) = s match {
      case Some(signatory) => {
        Future.successful(signatory)
      }
      case None => {
        val signatory = Signatory(BSONObjectID.generate.stringify, user.provider, user.name, user.avatar, user.signed,
          user.version)
        for {
          lastError <- collection.save(signatory, writeConcern = GetLastError(awaitJournalCommit = true))
        } yield {
          if (lastError.ok) {
            signatory
          } else {
            throw new RuntimeException("Unable to save signatory: " + lastError.message)
          }
        }
      }
    }

    for {
      signatory <- find
      toReturn <- returnOrSave(signatory)
    } yield toReturn
  }

  /**
   * Find the user with the given id.
   *
   * @param id The id of the user to find.
   * @return A future of the user, if found.
   */
  def findUser(id: String): Future[Option[Signatory]] = findUser(BSONObjectID(id))

  /**
   * Find the user with the given id.
   *
   * @param id The id of the user to find.
   * @return A future of the user, if found.
   */
  def findUser(id: BSONObjectID): Future[Option[Signatory]] = {
    collection.find(Json.obj("_id" -> id)).cursor[Signatory].headOption()
  }

  /**
   * Load all the people that have signed the manifesto, in reverse chronological order.
   */
  def loadSignatories(): Future[List[Signatory]] = {
    collection.find(Json.obj("signed" -> Json.obj("$exists" -> true))).sort(
      Json.obj("signed" -> -1)
    ).cursor[Signatory].toList()
  }

  /**
   * Sign the manifesto
   *
   * @param id The id of the user that is signing
   * @return The updated user if successful, or an error message if the user is not allowed to sign.
   */
  def sign(id: BSONObjectID, version: Version): Future[Either[String, Signatory]] = {

    findUser(id).flatMap {
      case Some(signatory) => signatory.signed match {
        case None => {
          val signed = DateTime.now
          collection.update(Json.obj("_id" -> id), Json.obj("$set" ->
            Json.obj(
              "signed" -> BSONDateTime(signed.getMillis),
              "version" -> version
            )
          ), GetLastError(awaitJournalCommit = true)).map {
            lastError =>
              if (lastError.ok) {
                Right(signatory.copy(signed = Some(signed), version = Some(version)))
              } else {
                throw new RuntimeException("Error signing: " + lastError.message)
              }
          }
        }
        case Some(signed) => Future.successful(Left("Already signed on " + signed))
      }

      case None => Future.successful(Left("Signatory not found"))
    }
  }

  /**
   * Remove a signature from the manifesto
   *
   * @param id The id of the user that is removing their signature
   * @return The updated user
   */
  def unsign(id: BSONObjectID): Future[Either[String, Signatory]] = {

    findUser(id).flatMap {
      case Some(signatory) => {
        collection.update(Json.obj("_id" -> id), Json.obj("$unset" ->
          Json.obj(
            "signed" -> 1,
            "version" -> 1
          )
        ), GetLastError(awaitJournalCommit = true)).map {
          lastError =>
            if (lastError.ok) {
              Right(signatory.copy(signed = None, version = None))
            } else {
              throw new RuntimeException("Error unsigning: " + lastError.message)
            }
        }
      }

      case None => Future.successful(Left("Signatory not found"))
    }
  }

}
