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
        var _identifier: String = identifier match {
          case Some(value: String) => value
          case None => ""
        }

        var data:Option[JsObject] = (obj \ "data").asOpt[JsObject]
        var _data:JsObject = data match {
          case Some(obj: JsObject) => obj
          case None => Json.obj()
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
              e.getMessage()
            }
          )
        ))
    }
  }

  def getPoint(accId: Long, id: Long) = Action { implicit request =>
    Point.findOneById(accId, id) map { point: Point =>
        var _id: Long = point.id.get
      
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
  def getPointByTypeOrIdentifier(accId: Long, _typeString: String, _identifier: String, _limit: Int, _offset: Int) = Action { implicit request =>
    var typeString: String = _typeString
    var identifier: String = _identifier
    var limit: Int = _limit
    var offset: Int = _offset
    try {
      if(typeString.length == 0 && identifier.length == 0){
        val (__typeString, __identifier, __l, __o) = Form(
          tuple(
            "type" -> optional(text),
            "identifier" -> optional(text),
            "limit" -> optional(number(1, 100)),
            "offset" -> optional(number(0))
          )
        ).bindFromRequest.get
        typeString = __typeString.getOrElse("")
        identifier = __identifier.getOrElse("")
        limit = __l.getOrElse(10)
        offset = __o.getOrElse(0)
      }

      if(typeString != "" && identifier != ""){
        var typeId: Long = -1
        Point.Type.get(typeString) match {
          case Some(id: Long) => typeId = id
          case _ => throw new Exception("point(identifier=%1$s, type=%2$s) type: '%2$s' isn't supported.".format(identifier, typeString))
        }
        Point.findOneByTypeIdAndIdentifier(accId, typeId, identifier) map { point: Point =>
          var _id: Long = point.id.get
      
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
      }else if(typeString != ""){
        var typeId: Long = -1
        Point.Type.get(typeString) match {
          case Some(id: Long) => typeId = id
          case _ => throw new Exception("point(identifier=?, type=%1$s) type: '%1$s' isn't supported.".format(typeString))
        }
        val list: List[Point] = Point.findAllByTypeId(accId, typeId, limit, offset)
        if(list.length == 0){
          Application.NotFoundJson(404, "Point not found")  
        }else{
          var array: JsArray = new JsArray()
          list.foreach { point: Point =>
            var _id: Long = point.id.get
            array = Json.obj( "point" -> Json.obj(
              "id" -> _id,
              "type" -> Point.TypeString(point.typeId),
              "identifier" -> point.identifier,
              "createdAt" -> point.createdAt,
              "updatedAt" -> point.updatedAt,
              "referencedAt" -> point.referencedAt,
              "data" -> point.data,
              "_url" -> routes.Graph.getPoint(accId, _id).absoluteURL()
            )) +: array
          }
          
          val current: String = routes.Graph.getPointByTypeOrIdentifier(accId).absoluteURL() + "?type=%s&limit=%d&offset=%d".format(typeString, limit, offset)
          val next = routes.Graph.getPointByTypeOrIdentifier(accId).absoluteURL() + "?type=%s&limit=%d&offset=%d".format(typeString, limit, offset+limit)
          val prev = routes.Graph.getPointByTypeOrIdentifier(accId).absoluteURL() + "?type=%s&limit=%d&offset=%d".format(typeString, limit, math.max(0,offset-limit) )
          // var next: String = ""
          // var prev: String = ""
          // if(list.length > offset + limit){
          //   next = routes.Graph.getPointByTypeOrIdentifier(accId).absoluteURL() + "?type=%s&limit=%d&offset%d".format(typeString, limit, offset+limit)
          // }else{
          //   next = current + '#'
          // }
          // val pages = offset / limit
          // if(pages > 0){
          //   prev = routes.Graph.getPointByTypeOrIdentifier(accId).absoluteURL() + "?type=%s&limit=%d&offset%d".format(typeString, limit, math.min(0,offset-limit) )
          // }else{
          //   prev = current + '#'
          // }
          var result: JsObject = Json.obj(
            "status" -> Json.obj(
                "code" -> 200,
                "message" -> ""
              ),
            "points" -> array,
            "_previous" -> prev,
            "_current" -> current,
            "_next" -> next
            )
          Ok(result)
        }
      }else if(identifier != ""){
        val list: List[Point] = Point.findAllByIdentifier(accId, identifier, limit, offset)
        if(list.length == 0){
          Application.NotFoundJson(404, "Point not found")  
        }else{
          var array: JsArray = new JsArray()
          list.foreach { point: Point =>
            var _id: Long = point.id.get
            array = Json.obj( "point" -> Json.obj(
              "id" -> _id,
              "type" -> Point.TypeString(point.typeId),
              "identifier" -> point.identifier,
              "createdAt" -> point.createdAt,
              "updatedAt" -> point.updatedAt,
              "referencedAt" -> point.referencedAt,
              "data" -> point.data,
              "_url" -> routes.Graph.getPoint(accId, _id).absoluteURL()
            )) +: array
          }
          
          val current: String = routes.Graph.getPointByTypeOrIdentifier(accId).absoluteURL() + "?identifier=%s&limit=%d&offset%d".format(identifier, limit, offset)
          var next: String = ""
          var prev: String = ""
          if(list.length > offset + limit){
            next = routes.Graph.getPointByTypeOrIdentifier(accId).absoluteURL() + "?identifier=%s&limit=%d&offset%d".format(identifier, limit, offset+limit)
          }else{
            next = current + '#'
          }
          val pages = offset / limit
          if(pages > 0){
            prev = routes.Graph.getPointByTypeOrIdentifier(accId).absoluteURL() + "?identifier=%s&limit=%d&offset%d".format(identifier, limit, math.min(0,offset-limit) )
          }else{
            prev = current + '#'
          }
          var result: JsObject = Json.obj(
            "status" -> Json.obj(
                "code" -> 200,
                "message" -> ""
              ),
            "points" -> array,
            "_previous" -> prev,
            "_current" -> current,
            "_next" -> next
            )
          Ok(result)
        }
      }else{
        throw new Exception("point(identifier=?, type=?) how can I do for you? ") 
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