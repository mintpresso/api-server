package controllers

import play.api._
import play.api.mvc._
import play.api.data.Forms._
import play.api.data._

import models._

import play.api.Play.current
import play.api.libs._
import play.api.libs.iteratee._
import play.api.libs.concurrent._
import java.util.concurrent._
import scala.concurrent.stm._
import play.api.cache._
import play.api.libs.json._
import play.api.libs.json.Json._

object Accounts extends Controller with Secured {

  def list = LocalAPI { implicit request =>
    Ok( Account.count.getOrElse(0) + " account(s)" )
  }
  def add(email: String, name: String, password: String) = LocalAPI { implicit request =>
    try {
      Account.findOneByEmail(email) map { acc =>
        Conflict(Json.obj(
          "status" -> Json.obj(
            "code" -> 409,
            "message" -> acc.name
          )
        ))
      } getOrElse {
        Account.add( Account(email, name, Crypto.sign(password) )) map { id: Long =>
          Account.updateToken(id, Crypto.encryptAES(id.toString + "*"))
          Ok(Json.obj(
            "status" -> Json.obj(
              "code" -> 201,
              "message" -> ""
            ),
            "account" -> Json.obj(
              "id" -> toJson(id),
              "email" -> toJson(email),
              "name" -> toJson(name)
            )
          ))  
        } getOrElse {
          BadRequest(Json.obj(
            "status" -> Json.obj(
              "code" -> 500,
              "message" -> "Account not created. try again"
            )
          ))
        }
      } 
    } catch { 
      case e: Exception =>
        BadRequest(Json.obj(
          "status" -> Json.obj(
            "code" -> 400,
            "message" -> e.getMessage()
          )
        ))
    }
  }
  def findOneById(id: Long) = LocalAPI { implicit request =>
    Account.findOneById(id) map { acc =>
      var _id: Long = -1
      
      acc.id.get match { 
        case x: Long => _id = x
        case _ => Application.NotFoundJson(404, "Account(id="+id+") not found")
      }
   
      Ok(Json.obj(
        "status" -> Json.obj(
          "code" -> 200,
          "message" -> ""
        ),
        "account" -> Json.obj(
          "id" -> _id,
          "email" -> acc.email,
          "name" -> acc.name
        )
      ))
    } getOrElse {
      Application.NotFoundJson(404, "Account(id="+id+") not found")
    }
  }

  def authenticate(email: String, pw: String) = LocalAPI { implicit request =>
    Account.findOneByEmail(email) map { acc =>
      val hash = Crypto.sign(pw)
      if(acc.password == hash){
        var _id: Long = acc.id.get match {
          case x: Long => x
        }

        Ok(Json.obj(
          "status" -> Json.obj(
            "code" -> 200,
            "message" -> ""
          ),
          "account" -> Json.obj(
            "id" -> _id,
            "email" -> acc.email,
            "name" -> acc.name,
            "api_token" -> acc.api_token
          )
        ))
      }else{
        Forbidden
      }
    } getOrElse {
      NoContent
    }
  }

  def setAPI(id: Long, pw: String, url: String) = LocalAPI { implicit request =>
    Account.findOneById(id) map { acc =>
      if(acc.password == Crypto.sign(pw)){
        Account.updateToken( id, Crypto.encryptAES(id + url) )
        Ok
      }else{
        Forbidden
      }
    } getOrElse {
      Forbidden
    }
  }

  def getAPI(id: Long) = LocalAPI { implicit request =>
    Account.findOneById(id) map { acc =>
      val t = acc.api_token
      val d = Crypto.decryptAES(t)
      val ll = id.toString.length
      val i = d.substring(0, ll)
      if(i == id.toString){
        val u: Array[String] = d.substring(ll).split(",")
        Ok(Json.obj(
          "token" -> acc.api_token,
          "urls" -> u
          ))
      }else{
        Forbidden
      }
    } getOrElse {
      NotFound
    }
  }
}