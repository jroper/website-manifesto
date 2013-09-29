package controllers

import play.api.mvc.{Action, Controller}
import org.apache.commons.codec.digest.DigestUtils
import play.api.templates.Html
import models.Version

/**
 * Serves the main pages of the application
 */
object Application extends Controller {

  // Serves the content with an E-Tag, and checks if the E-Tag matches
  def cached(html: Html) = {
    // Hash the content
    val hash = DigestUtils.md5Hex(html.body)
    Action { req =>
      req.headers.get(IF_NONE_MATCH).collect {
        case `hash` => NotModified
      } getOrElse {
        Ok(html).withHeaders(ETAG -> hash)
      }
    }
  }

  /**
   * The index page.
   */
  val index = cached(Version.latest.template())

  /**
   * A specific version of the manifesto.
   */
  def version(name: String) = {
    Version.fromName(name) match {
      case Some(version) => cached(version.template())
      case None => Action(Redirect(routes.Application.index()))
    }
  }

  /**
   * The list page.
   */
  val list = cached(views.html.list())
}
