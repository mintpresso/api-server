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
  val mintpresso: Affogato = Affogato( Play.configuration.getString("mintpresso.internal.api").getOrElse(""), Play.configuration.getString("mintpresso.internal.id").getOrElse("0").toLong)
  private var accountId: Long = -1L

  def getAccountId(): Long = accountId
  def SignedAPI(accessId: Long)(f: Request[AnyContent] => Result) = Action { implicit request =>
    val domain = Play.configuration.getString("mintpresso.panel.domain").getOrElse("localhost")
    val remoteAddress = Play.configuration.getString("mintpresso.panel.address").getOrElse("127.0.0.1")
    if(request.domain == domain && request.remoteAddress == remoteAddress){
      f(request)
    }else{
      mintpresso.get(accessId) match {
        case Some(account) => {
          var key = request.queryString.get("api_token").flatMap(_.headOption).getOrElse("")
          if(key.length == 0){
            Logger.info("Account("+account.identifier+") zero-length key")
            Results.Forbidden
          }else{
            val len = accessId.toString.length
            val part = key.substring(0, len)
            if(part != len.toString){
              Logger.info("Account("+account.identifier+") signed id unmatch")
              Results.Forbidden
            }else{
              // get token
              mintpresso.get("token", key).as[Option[Point]] match {
                case Some(token) => {
                  // parse domain list
                  val json = Json.parse(token.data)
                  // check whether it has been expired
                  if( (json \ "expired").asOpt[Boolean].getOrElse(true) == true ){
                    Logger.info("Account("+account.identifier+") token("+key+") expired")
                    Results.Forbidden
                  }else{
                    val urls: Array[String] = (json \ "url").asOpt[String].getOrElse("*").split("|")
                    val list: Array[String] = (json \ "address").asOpt[String].getOrElse("").split("|")
                    // allow all(*) or given addresses
                    if( urls.contains("*") || list.contains(remoteAddress)){
                      f(request)
                    }else{
                      Logger.info("Account("+account.identifier+") token("+key+") address("+remoteAddress+") denied")
                      Results.Forbidden
                    }
                  }
                }
                case None => {
                  Logger.info("Account("+account.identifier+") token("+key+") not found")
                  Results.Forbidden
                }
              }
            }
          }
        }
        case None => {
          Logger.info("Account(None)")
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