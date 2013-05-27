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
      // logging will be recursive
      // mintpresso.set(Map[Symbol, String](
      //   'request -> java.util.UUID.randomUUID().toString,
      //   'domain -> request.domain,
      //   'remoteAddress -> request.remoteAddress,
      //   'url -> request.uri,
      //   'local -> "true"
      // )).as[Option[Point]] match {
      //   case Some(point) => mintpresso.set("user", "support@mintpresso.com", "log", "request", point.identifier)
      //   case None => {}
      // }
      f(request)
    }else{
      mintpresso.get(accessId) match {
        case Some(account) => {
          var key = request.queryString.get("api_token").flatMap(_.headOption).getOrElse("")
          if(key.length == 0){
            val uuid = java.util.UUID.randomUUID().toString
            val p = Point(0, "warning", uuid, Json.obj(
                "message" -> "zero-length key",
                "domain" -> request.domain,
                "remoteAddress" -> request.remoteAddress,
                "url" -> request.uri,
                "token" -> key
              ).toString, "", 0, 0, 0)

            mintpresso.set(p) match {
              case Some(point) => mintpresso.set("user", account.identifier, "log", "warning", uuid)
              case None => Logger.info("Not logged. Account("+account.identifier+") zero-length key")
            }
            Results.Forbidden
          }else{
            val len = accessId.toString.length
            val part = key.substring(0, len)
            if(part != len.toString){
              val uuid = java.util.UUID.randomUUID().toString
              val p = Point(0, "warning", uuid, Json.obj(
                "message" -> "signed id unmatch",
                "domain" -> request.domain,
                "remoteAddress" -> request.remoteAddress,
                "url" -> request.uri,
                "token" -> key
              ).toString, "", 0, 0, 0)

              mintpresso.set(p) match {
                case Some(point) => mintpresso.set("user", account.identifier, "log", "warning", uuid)
                case None => Logger.info("Not logged. Account("+account.identifier+") signed id unmatch")
              }
              Results.Forbidden
            }else{
              // get token
              mintpresso.get("token", key).as[Option[Point]] match {
                case Some(token) => {
                  // parse domain list
                  val json = Json.parse(token.data)
                  // check whether it has been expired
                  if( (json \ "expired").asOpt[Boolean].getOrElse(true) == true ){
                    val uuid = java.util.UUID.randomUUID().toString
                    val p = Point(0, "warning", uuid, Json.obj(
                      "message" -> "token expired",
                      "domain" -> request.domain,
                      "remoteAddress" -> request.remoteAddress,
                      "url" -> request.uri,
                      "token" -> key
                    ).toString, "", 0, 0, 0)

                    mintpresso.set(p) match {
                      case Some(point) => mintpresso.set("user", account.identifier, "log", "warning", uuid)
                      case None => Logger.info("Not logged. Account("+account.identifier+") token("+key+") expired")
                    }
                    Results.Forbidden
                  }else{
                    val urls: Array[String] = (json \ "url").asOpt[String].getOrElse("*").split("|")
                    val list: Array[String] = (json \ "address").asOpt[String].getOrElse("").split("|")
                    // allow all(*) or given addresses
                    if( urls.contains("*") || list.contains(remoteAddress)){
                      val uuid = java.util.UUID.randomUUID().toString
                      
                      // var json = Json.obj(
                      //   "domain" -> request.domain
                      // )
                      // println(Point(0, "request", uuid, json.toString, "", 0, 0, 0))
                      // var p = Point(0, "request", uuid, json.toString, "", 0, 0, 0)
                      // mintpresso.set(p) match {
                      //   case Some(point) => mintpresso.set("user", account.identifier, "log", "request", uuid)
                      //   case None => Logger.info("Account("+account.identifier+") token("+key+") address("+remoteAddress+") requested")
                      // }

                      mintpresso.set(Map[Symbol, String](
                        'request -> uuid,
                        'message -> "OK",
                        'domain -> request.domain, 'remoteAddress -> request.remoteAddress, 'url -> request.uri, 'token -> key
                      )).as[Option[Point]] match {
                        case Some(point) => mintpresso.set("user", account.identifier, "log", "request", uuid)
                        case None => Logger.info("Account("+account.identifier+") token("+key+") address("+remoteAddress+") requested")
                      }

                      // mintpresso.set(Map[String, String](
                      //   "request" -> uuid,
                      //   "domain" -> request.domain, 
                      //   "remoteAddress" -> request.remoteAddress,
                      //   "url" -> request.uri,
                      //   "token" -> key
                      // )).as[Option[Point]] match {
                      //   case Some(point) => mintpresso.set("user", account.identifier, "log", "request", uuid)
                      //   case None => Logger.info("Account("+account.identifier+") token("+key+") address("+remoteAddress+") requested")
                      // }
                      f(request)
                    }else{
                      val uuid = java.util.UUID.randomUUID().toString
                      val p = Point(0, "warning", uuid, Json.obj(
                        "message" -> "address denied",
                        "domain" -> request.domain,
                        "remoteAddress" -> request.remoteAddress,
                        "url" -> request.uri,
                        "token" -> key
                      ).toString, "", 0, 0, 0)

                      mintpresso.set(p) match {
                        case Some(point) => mintpresso.set("user", account.identifier, "log", "warning", uuid)
                        case None => Logger.info("Account("+account.identifier+") token("+key+") address("+remoteAddress+") denied")
                      }
                      Results.Forbidden
                    }
                  }
                }
                case None => {
                  val uuid = java.util.UUID.randomUUID().toString
                  mintpresso.set(Map[Symbol, String](
                    'warning -> uuid,
                    'message -> "token not found",
                    'domain -> request.domain, 'remoteAddress -> request.remoteAddress, 'url -> request.uri, 'token -> key
                  )).as[Option[Point]] match {
                    case Some(point) => mintpresso.set("user", account.identifier, "log", "warning", uuid)
                    case None => Logger.info("Account("+account.identifier+") token("+key+") not found")
                  }
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