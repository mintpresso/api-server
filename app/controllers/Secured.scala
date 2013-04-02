package controllers

import play.api._
import play.api.mvc._
import play.api.data.Forms._
import play.api.data._
import play.api.Play.current
import play.api.libs._

import play.api.libs.concurrent._
import java.util.concurrent._
import play.api.libs.json._

import com.mintpresso._
import play.api.libs.concurrent.Execution.Implicits._
/** Uncomment the following lines as needed **/
/**
import play.api.libs.iteratee._
import scala.concurrent.stm._
import akka.util.duration._
import play.api.cache._
**/

trait Secured {
  private var accountId: Long = -1L

  def getAccountId(): Long = accountId
  def SignedAPI(accessId: Long)(f: Request[AnyContent] => Result) = Action { implicit request =>
    val domain = Play.configuration.getString("mintpresso.panel.domain").getOrElse("localhost")
    val remoteAddress = Play.configuration.getString("mintpresso.panel.address").getOrElse("127.0.0.1")
    if(request.domain == domain && request.remoteAddress == remoteAddress){
      f(request)
    }else{
      request.queryString.get("api_token").flatMap(_.headOption) match {
        case Some(token) => {
          if(token.length == 0){
            Logger.info("Access Denied: zero-length token")
            Results.Forbidden
          }else{
            val ll = accessId.toString.length
            val i = token.substring(0, ll)
            if(i != accessId.toString){
              Logger.warn("Given ID is unmatched with the id signed in token.")
              Results.Forbidden
            }else{
              Results.Async {
                MintpressoAPI("internal").findByTypeAndIdentifier("token", token).map { res =>
                  res.status match {
                    case 200 => {
                      val json = Json.parse(res.body)
                      val expired = (json \ "point" \ "data" \ "expired").as[Boolean]
                      val urls: List[String] = (json \ "point" \ "data" \ "url").as[String].split('|').toList
                      if(expired){
                        Logger.info("FAILED - expired token " + res.body)
                        Results.Forbidden
                      }else{
                        Logger.info("COMPARE DISABLED: domain(" + request.domain + "), remoteAddress(" + request.remoteAddress + "), filter(" + urls.toString + ") ")
                        f(request)
                      }
                    }
                    case _ => {
                      Logger.info("FAILED - find token")
                      Results.Forbidden
                    }
                  }
                }
              }
            }
          }
        }
        case None => {
          Logger.info("Access Denied: no token given")
          Results.Forbidden
        }
      }
    }
  }
  def LocalAPI(f: Request[AnyContent] => Result) = Action { implicit request =>
    // allows access from MintpressoCore
    val domain = Play.configuration.getString("mintpresso.panel.domain").getOrElse("localhost")
    val remoteAddress = Play.configuration.getString("mintpresso.panel.address").getOrElse("127.0.0.1")
    if(request.domain == domain && request.remoteAddress == remoteAddress){
      f(request)
    }else{
      Logger.info("Access Denied: domain " + request.domain + " == " + domain + " && remoteAddress " + request.remoteAddress + " == " + remoteAddress)
      Results.Forbidden
    }
  }
}