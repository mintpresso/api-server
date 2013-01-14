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
import anorm._ 

object Graph extends Controller {

  /*
  {
    "point": {
      "type": "user",
      "identifier": "eces@mstock.org",
      "data": {
        "plan": "startup"
      }
    }
  }
   */
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

        var point: Point = Point(accId, typeString.get, _identifier, _data)
        var code: Int = 0
        var msg: String = ""
        point.id match {
          case NotAssigned => {
            Point.add(point) map { id: Long =>
              point.id = new Id(id)
              code = 201
              msg = "Point created"
            } getOrElse {
              InternalServerError(Json.obj(
                "status" -> Json.obj(
                  "code" -> 500,
                  "message" -> "Point not created. try again"
                )
              ))
            }
          }
          case id: Pk[Long] => {
            code = 200
            msg = "Already defined."
          }
        }

        Ok(Json.obj(
          "status" -> Json.obj(
            "code" -> code,
            "message" -> msg
          ),
          "point" -> Json.obj(
            "id" -> point.id.get,
            "type" -> typeString.get,
            "identifier" -> _identifier,
            "data" -> _data,
            "_url" -> routes.Graph.getPoint(accId, point.id.get).absoluteURL()
          )
        ))  
        
      } getOrElse {
        throw new Exception("""Json object 'point' is required like this: { "point": ... } """)
      }
    } catch { 
      case e: Exception =>
        BadRequest(Json.obj(
          "status" -> Json.obj(
            "code" -> 400,
            "message" -> {
              e.printStackTrace()
              e.getMessage()
            }
          )
        ))
    }
  }

  def getPoint(accId: Long, id: Long) = Action { implicit request =>
    Point.findOneById(accId, id) map { point: Point =>
        var _id: Long = -1
      
        point.id.get match { 
          case x: Long => _id = x
          case _ => {
            InternalServerError(Json.obj(
              "status" -> Json.obj(
                "code" -> 500,
                "message" -> "(accId=%s / pointId=%s) doesn't have Pk[Id]".format(accId, id)
              )
            ))
          }
        }
        Ok(Json.obj(
          "status" -> Json.obj(
            "code" -> 200,
            "message" -> ""
          ),
          "point" -> Json.obj(
            "id" -> _id,
            "type" -> Point.TypeString(point.typeId),
            "identifier" -> point.identifier,
            "createdAt" -> point.createdAt,
            "updatedAt" -> point.updatedAt,
            "referencedAt" -> point.referencedAt,
            "data" -> (point.data \ "data"),
            "_url" -> routes.Graph.getPoint(accId, _id).absoluteURL()
          )
        ))  
      } getOrElse {
        Application.NotFoundJson(404, "Point not found")
      }
  }
  def getPointByTypeAndIdentifier(accId: Long, typeString: String, identifier: String) = Action { implicit request =>
    try {
      var typeId: Long = -1
      Point.Type.get(typeString).get match {
        case id: Long => typeId = id
        case _ => throw new Exception("point(identifier=" + identifier + ", type=?) type: '" + typeString + "' isn't supported.")
      }

      if(identifier.length > 0){
        Point.findOneByTypeIdAndIdentifier(accId, Point.Type(typeString), identifier) map { point: Point =>
          var _id: Long = -1
      
          point.id.get match { 
            case x: Long => _id = x
            case _ => {
              InternalServerError(Json.obj(
                "status" -> Json.obj(
                  "code" -> 500,
                  "message" -> "(accId=%s / typeString=%s / identifer=%s) doesn't have Pk[Id]".format(accId, typeString, identifier)
                )
              ))
            }
          }
          Ok(Json.obj(
            "status" -> Json.obj(
              "code" -> 200,
              "message" -> ""
            ),
            "point" -> Json.obj(
              "id" -> _id,
              "type" -> Point.TypeString(point.typeId),
              "identifier" -> point.identifier,
              "createdAt" -> point.createdAt,
              "updatedAt" -> point.updatedAt,
              "referencedAt" -> point.referencedAt,
              "data" -> point.data,
              "_url" -> routes.Graph.getPoint(accId, _id).absoluteURL()
            )
          ))  
        } getOrElse {
          Application.NotFoundJson(404, "Point not found")
        }
      }else{
        throw new Exception("point(identifier=?, type='" + typeString + "') identifier isn't given.")
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