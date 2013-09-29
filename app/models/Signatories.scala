package models

import reactivemongo.bson._
import play.modules.reactivemongo.json.BSONFormats._
import org.joda.time.DateTime
import play.api.libs.json._
import play.api.libs.functional.syntax._
import play.api.templates.Html
import org.joda.time.format.DateTimeFormat

/**
 * A signatory
 *
 * @param id The internal id of the signatory
 * @param provider The provider specific ID of the signatory
 * @param name The name
 * @param avatarUrl The URL of the signatories avatar
 * @param signed The date the signatory signed, if they signed
 */
case class Signatory(
  id: String,
  provider: Provider,
  name: String,
  avatarUrl: Option[String],
  signed: Option[DateTime],
  version: Option[Version]
)

/**
 * A login provider
 */
sealed trait Provider

case class GitHub(id: Long, login: String) extends Provider
case class Twitter(id: Long, screenName: String) extends Provider
case class Google(id: String) extends Provider
case class LinkedIn(id: String) extends Provider

/**
 * A version of the manifesto
 */
case class Version private (name: String, releaseDate: DateTime, template: () => Html)

object Version {

  private val format = DateTimeFormat.forPattern("yyyy-MM-dd")

  val v1_0 = new Version("1.0", format.parseDateTime("2013-07-15"), () => views.html.manifesto_1_0())
  val v1_1 = new Version("1.1", format.parseDateTime("2013-09-23"), () => views.html.manifesto_1_1())

  val all = Seq(v1_0, v1_1)
  val latest = v1_1
  def fromName(name: String) = name match {
    case "1.0" => Some(v1_0)
    case "1.1" => Some(v1_1)
    case other => None
  }

  implicit val versionFormat = Format[Version](
    Reads(js => js.validate[String].flatMap(name => fromName(name) match {
      case Some(version) => JsSuccess(version)
      case None => JsError("Unknown version: " + name)
    })),
    Writes[Version](v => JsString(v.name))
  )
}

// Formats
object Signatory {
  implicit val signatoryFormat = Json.format[Signatory]

  val dbSignatoryFormat: Format[Signatory] = (
    (__ \ "_id").format[BSONObjectID] ~
    (__ \ "provider").format[Provider] ~
    (__ \ "name").format[String] ~
    (__ \ "avatarUrl").formatNullable[String] ~
    (__ \ "signed").formatNullable[BSONDateTime] ~
    (__ \ "version").formatNullable[Version]
  ).apply({
    case (id, provider, name, avatarUrl, signed, version) =>
      val migratedVersion = version.orElse {
        signed.map {
          case v1_1 if Version.v1_1.releaseDate.getMillis < v1_1.value => Version.v1_1
          case v1_0 => Version.v1_0
        }
      }
      Signatory(id.stringify, provider, name, avatarUrl, signed.map(dt => new DateTime(dt.value)), migratedVersion)
  }, (s: Signatory) =>
    (BSONObjectID(s.id), s.provider, s.name, s.avatarUrl, s.signed.map(dt => BSONDateTime(dt.getMillis)), s.version)
  )
}

object Provider {
  private def readProvider[T : Reads](json: JsValue): JsSuccess[T] = JsSuccess((json \ "details").as[T])
  private def writeProvider[T : Writes](id: String, provider: T) = Json.obj(
    "id" -> id,
    "details" -> provider
  )

  implicit val providerFormat: Format[Provider] = Format[Provider](
    Reads(json => (json \ "id").asOpt[String] match {
      case Some("github") => readProvider[GitHub](json)
      case Some("twitter") => readProvider[Twitter](json)
      case Some("google") => readProvider[Google](json)
      case Some("linkedin") => readProvider[LinkedIn](json)
      case unknown => JsError("Unknown provider: " + unknown)
    }),
    Writes {
      case gh: GitHub => writeProvider("github", gh)(GitHub.githubFormat)
      case t: Twitter => writeProvider("twitter", t)(Twitter.twitterFormat)
      case g: Google => writeProvider("google", g)(Google.googleFormat)
      case ln: LinkedIn => writeProvider("linkedin", ln)(LinkedIn.linkedInFormat)
    }
  )
}

object GitHub {
  implicit val githubFormat: Format[GitHub] = Json.format[GitHub]
}

object Twitter {
  implicit val twitterFormat: Format[Twitter] = Json.format[Twitter]
}

object Google {
  implicit val googleFormat: Format[Google] = Json.format[Google]
}

object LinkedIn {
  implicit val linkedInFormat: Format[LinkedIn] = Json.format[LinkedIn]
}


