package controllers

import play.api._
import play.api.mvc._
import play.api.data.Forms._
import play.api.data._
import play.api.Play.current
import play.api.libs._
import models.Account

/** Uncomment the following lines as needed **/
/**
import play.api.libs.iteratee._
import play.api.libs.concurrent._
import java.util.concurrent._
import scala.concurrent.stm._
import akka.util.duration._
import play.api.cache._
import play.api.libs.json._
**/

trait Secured {
  private var accountId: Long = -1L

  def getAccountId(): Long = accountId
  def SignedAPI(accessId: Long)(f: Request[AnyContent] => Result) = Action { implicit request =>
    val (_api_token, _empty) = Form(
      tuple(
        "api_token" -> optional(text),
        "empty" -> optional(text)
      )
    ).bindFromRequest.get
    val api_token = _api_token.getOrElse("")
    if(api_token.length == 0 || !Account.verifyToken(accessId, api_token, request)){
      Results.Forbidden
    } else {
      f(request)
    }
  }
  def LocalAPI(f: Request[AnyContent] => Result) = Action { implicit request =>
    // allows access from MintpressoCore
    val domain = Play.configuration.getString("mintpresso.panel.domain").getOrElse("localhost")
    val remoteAddress = Play.configuration.getString("mintpresso.panel.address").getOrElse("127.0.0.1")
    (request.domain, request.remoteAddress) match {
      case (domain, remoteAddress) => f(request)
      case _ => {
        Logger.info("COMPARE: domain(" + request.domain + "), remoteAddress(" + request.remoteAddress + ") with " + (Play.configuration.getString("mintpresso.panel.domain"), Play.configuration.getString("mintpresso.panel.address")))
        Results.Forbidden
      }
    }
  }
}