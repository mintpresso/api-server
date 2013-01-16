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

object Accounts extends Controller {

  def list = Action {
    Ok(
      Account.count.getOrElse(0) + " account(s)"
    )
  }
  def add(email: String, name: String, password: String) = Action {
    try {
      Account.add( Account(email, name, password) ) map { id: Long =>
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
  def findOneById(id: Long) = Action { implicit request =>
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

}