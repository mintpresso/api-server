package controllers

import play.api._
import play.api.mvc._
import play.api.data.Forms._
import play.api.data._

import play.api.Play.current
import play.api.libs._
import play.api.cache._
import play.api.libs.json._

import models._

object Graph extends Controller {

  def addPoint(accId: Long) = Action(parse.json) { implicit request =>
    try { 
      (request.body \ "point").asOpt[JsObject].map { obj =>
        var typeString = (obj \ "type").asOpt[String]
        if(typeString.get == None){
          throw new Exception("""Json object 'type' is required like this: { "point": {"type": ... } } """)
        }
        var identifier = (obj \ "identifier").asOpt[String]
        var _identifier: String = ""
        if(identifier.get != None){
          _identifier = identifier.get
        }
        var data:Option[JsObject] = (obj \ "data").asOpt[JsObject]
        var _data:JsObject = Json.obj()
        if(data.get != None){
          _data = data.get
        }
        Point.add( Point(accId, typeString.get, _identifier, _data) ) map { id: Long =>
          Ok(Json.obj(
            "status" -> Json.obj(
                "code" -> 201,
                "message" -> ""
            ),
            "point" -> Json.obj(
              "id" -> id,
              "type" -> typeString.get,
              "identifier" -> _identifier,
              "data" -> _data
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
      } getOrElse {
        throw new Exception("""Json object 'point' is required like this: { "point": ... } """)
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

}