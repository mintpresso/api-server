package controllers

import play.api._
import play.api.mvc._
import play.api.data.Forms._
import play.api.data._

import play.api.Play.current
import play.api.libs._
import play.api.cache._
import play.api.libs.json._
import play.api.libs.functional.syntax._

import models._
import anorm._ 

object Graph extends Controller with Secured {

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
  def addPoint(accId: Long) = SignedAPI(accId) { implicit request =>
    try {
      var content = request.body.asJson
      request.queryString.get("json").flatMap(_.headOption) match {
        case Some(json) => {
          content = Some(Json.parse(json))
        }
        case None => 
      }
      var updateIfExists = request.queryString.get("updateIfExists").flatMap(_.headOption).getOrElse("false").toBoolean
      content.map { json =>
        (json \ "point").asOpt[JsObject].map { obj =>
          var code: Int = 0
          var msg: String = ""

          var Word = """(\w+)""".r
          var typeString = ""
          (obj \ "type").asOpt[String] match {
            case Some(Word(ts)) => typeString = ts
            case None => throw new Exception("""Json object 'type' is required like this: { "point": {"type": ... } } """)
            case _ => throw new Exception("""Json object 'type' is illegal. point.type should be combination of word and number. """)
          } 

          var identifier = (obj \ "identifier").asOpt[String]
          var _identifier: String = identifier match {
            case Some(value: String) => value
            case None => ""
          }

          var data:Option[JsObject] = (obj \ "data").asOpt[JsObject]
          var _data:JsObject = data match {
            case Some(obj: JsObject) => obj
            case None => {
              msg += "json.invalid"
              Json.obj()
            }
          }

          var point: Point = Point(accId, typeString, _identifier, _data)
          point.id match {
            case NotAssigned => {
              Point.add(point) map { id: Long =>
                point.id = new Id(id)
                code = 201
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
              if(updateIfExists){
                // check data is changed
                if(_data.toString == point.data.toString){
                  code = 200
                  msg += "No changes"
                }else{
                  import java.util.Date
                  val now = new Date

                  // duplicate from original point without lock
                  val archive: Point = Point(NotAssigned, accId, point.typeId, point.identifier+'['+point.updatedAt.getTime+']', point.createdAt, point.updatedAt, now, point.data)
                  Point.add(archive) match {
                    case Some(archiveId) => {
                      val archiveEdge = Edge(accId, point.id.get, point.typeId, "archive", archiveId, point.typeId)
                      Edge.add(archiveEdge)
                      Point.referenced(point.id.get, now)
                      point.referencedAt = now

                      // update data
                      point.data = _data
                      Point.update(point)
                      Point.updated(point.id.get, now)
                      point.updatedAt = now
                      code = 201
                      msg += "Updated."
                    }
                    case None => {
                      InternalServerError(Json.obj(
                        "status" -> Json.obj(
                          "code" -> 500,
                          "message" -> "Not updated completely. Nothing changed. try again"
                        )
                      ))
                    }
                  }
                  
                }

              }else{
                code = 200
                msg += "Already defined. "
              }
            }
          }

          val result: JsObject = Json.obj(
            "status" -> Json.obj(
              "code" -> code,
              "message" -> msg
            ),
            "point" -> Json.obj(
              "id" -> point.id.get,
              "type" -> typeString,
              "identifier" -> _identifier,
              "createdAt" -> point.createdAt.getTime,
              "updatedAt" -> point.updatedAt.getTime,
              "referencedAt" -> point.referencedAt.getTime,
              "data" -> _data,
              "_url" -> routes.Graph.getPoint(accId, point.id.get).absoluteURL()
            )
          )  
          
          request.queryString.get("callback").flatMap(_.headOption) match {
            case Some(callback) => {
              if(code == 200){
                Ok(Jsonp(callback, result))
              }else{
                // 201
                Created(Jsonp(callback, result))
              }
            }
            case None => {
              if(code == 200){
                Ok(result)
              }else{
                // 201
                Created(result)
              }
            }
          }
          
        } getOrElse {
          throw new Exception("""Json object 'point' is required like this: { "point": ... } """)
        }
      } getOrElse {
        throw new Exception("""Json object 'point' is required like this: { "point": ... }. Make sure "Content-Type: application/json" on header. """)
      }
    } catch { 
      case e: Exception =>
        val json = Json.obj(
          "status" -> Json.obj(
            "code" -> 400,
            "message" -> {
              e.getMessage()
            }
          )
        )
        request.queryString.get("callback").flatMap(_.headOption) match {
          case Some(callback) => Ok(Jsonp(callback, json))
          case None => BadRequest(json)
        }
    }
  }

  def getPoint(accId: Long, id: Long) = SignedAPI(accId) { implicit request =>
    Point.findOneById(accId, id) map { point: Point =>
      PointType.findOneById(point.typeId) map { pointType: PointType =>
        var _id: Long = point.id.get
      
        val json = Json.obj(
          "status" -> Json.obj(
            "code" -> 200,
            "message" -> ""
          ),
          "point" -> Json.obj(
            "id" -> _id,
            "type" -> pointType.name,
            "identifier" -> point.identifier,
            "createdAt" -> point.createdAt,
            "updatedAt" -> point.updatedAt,
            "referencedAt" -> point.referencedAt,
            "data" -> point.data,
            "_url" -> routes.Graph.getPoint(accId, _id).absoluteURL()
          )
        )
        request.queryString.get("callback").flatMap(_.headOption) match {
          case Some(callback) => Ok(Jsonp(callback, json))
          case None => Ok(json)
        }
      } getOrElse {
        var json = Json.obj(
          "status" -> Json.obj(
            "code" -> 400,
            "message" -> "point(identifier=%1$s) is invalid.".format(point.identifier)
          )
        )
        request.queryString.get("callback").flatMap(_.headOption) match {
          case Some(callback) => Ok(Jsonp(callback, json))
          case None => Ok(json)
        }
      }
    } getOrElse {
      request.queryString.get("callback").flatMap(_.headOption) match {
        case Some(callback) => Ok(Jsonp(callback, Application.JsonStatus(404)))
        case None => Application.NotFoundJson()
      }
    }
  }

  def getPointTypes(accId: Long) = SignedAPI(accId) { implicit request =>
    val listOfPointTypes: Seq[JsString] = PointType.findAllByAccountId(accId) map(JsString)
    val jsPointTypes = new JsArray(listOfPointTypes)
    Ok(jsPointTypes)
  }

  def getPointLatest(accId: Long) = SignedAPI(accId) { implicit request =>
    val list: List[Point] = Point.findAllByLatest(accId)
    if(list.length == 0){
      Application.NotFoundJson(404, "Point not found")  
    }else{
      try {
        var array: JsArray = new JsArray()
        list.foreach { point: Point =>
          PointType.findOneById(point.typeId) map { pt =>
            var _id: Long = point.id.get
            array = Json.obj(
              "id" -> _id,
              "type" -> pt.name,
              "identifier" -> point.identifier,
              "createdAt" -> point.createdAt,
              "updatedAt" -> point.updatedAt,
              "referencedAt" -> point.referencedAt,
              "data" -> point.data,
              "_url" -> routes.Graph.getPoint(accId, _id).absoluteURL()
            ) +: array
          } getOrElse {
            throw new Exception("point type of '%1$s' cannot be found. point(identifier=%1$s) is invalid.".format(point.identifier)) 
          }
        }
        var result: JsObject = Json.obj(
          "status" -> Json.obj(
              "code" -> 200,
              "message" -> ""
            ),
          "points" -> array
          )
        Ok(result)
      } catch {
        case e: Exception => {
          e.printStackTrace()
          val json = Json.obj(
            "status" -> Json.obj(
              "code" -> 400,
              "message" -> {
                e.getMessage()
              }
            )
          )
          BadRequest(json)
        }
      }
    }
  }
  def getPointByTypeOrIdentifier(accId: Long) = SignedAPI(accId) { implicit request =>
    var typeString: String = request.queryString.get("type").flatMap(_.headOption).getOrElse("").toString
    var identifier: String = request.queryString.get("identifier").flatMap(_.headOption).getOrElse("").toString
    val Number = "([0-9]+)".r
    var limit: Int = 100
    var offset: Int = 0
    request.queryString.get("limit").flatMap(_.headOption).getOrElse(10) match {
      case Number(_s) => {
        limit = _s.toInt
      }
      case _ => 
    }
    request.queryString.get("offset").flatMap(_.headOption).getOrElse(0) match {
      case Number(_s) => {
        offset = _s.toInt
      }
      case _ => 
    }
    try {
      if(typeString != "" && identifier != ""){
        var pointType: PointType = null
        PointType.findOneByName(typeString) map { pt =>
          pointType = pt
        } getOrElse {
          throw new Exception("point(identifier=%1$s, type=%2$s) type: '%2$s' isn't supported.".format(identifier, typeString))
        }

        Point.findOneByTypeIdAndIdentifier(accId, pointType.id.get, identifier) map { point: Point =>
          var _id: Long = point.id.get
          val json = Json.obj(
            "status" -> Json.obj(
              "code" -> 200,
              "message" -> ""
            ),
            "point" -> Json.obj(
              "id" -> _id,
              "type" -> pointType.name,
              "identifier" -> point.identifier,
              "createdAt" -> point.createdAt,
              "updatedAt" -> point.updatedAt,
              "referencedAt" -> point.referencedAt,
              "data" -> point.data
            ),
            "_length" -> 1,
            "length" -> 1
          )
          request.queryString.get("callback").flatMap(_.headOption) match {
            case Some(callback) => Ok(Jsonp(callback, json))
            case None => Ok(json)
          }  
        } getOrElse {
          request.queryString.get("callback").flatMap(_.headOption) match {
            case Some(callback) => Ok(Jsonp(callback, Application.JsonStatus(404, "Point not found")))
            case None => Application.NotFoundJson(404, "Point not found")
          }
        }
      }else if(typeString != ""){
        var typeId: Long = -1
        PointType.findOneByName(typeString) map { pt =>
          typeId = pt.id.get
        } getOrElse {
          throw new Exception("point(identifier=?, type=%1$s) type: '%1$s' isn't supported.".format(typeString))
        }
        val list: List[Point] = Point.findAllByTypeId(accId, typeId, limit, offset)
        val total = Point.countByTypeId(accId, typeId)

        if(list.length == 0){
          request.queryString.get("callback").flatMap(_.headOption) match {
            case Some(callback) => Ok(Jsonp(callback, Application.JsonStatus(404, "Point not found. Out of %d.".format(total))))
            case None => Application.NotFoundJson(404, "Point not found. Out of %d.".format(total))
          }
        }else{
          var array: JsArray = new JsArray()

          list.foreach { point: Point =>
            var _id: Long = point.id.get
            var ptn: String = null
            PointType.findOneById(point.typeId).map { pt =>
              ptn = pt.name
            }.getOrElse {
              throw new Exception("point(typeId=%1$d) isn't supported.".format(point.typeId))
            }
            array +:= Json.obj(
              "id" -> _id,
              "type" -> ptn,
              "identifier" -> point.identifier,
              "createdAt" -> point.createdAt,
              "updatedAt" -> point.updatedAt,
              "referencedAt" -> point.referencedAt,
              "data" -> point.data
            )
          }

          
          var result: JsObject = Json.obj(
            "status" -> Json.obj(
                "code" -> 200,
                "message" -> ""
              ),
            "points" -> array,
            "_length" -> list.length, 
            "length" -> list.length,
            "size" -> total
          )
          result ++= Json.obj("url" -> (routes.Graph.getPointByTypeOrIdentifier(accId).absoluteURL() + "?type=%s".format(typeString)))
          result ++= Json.obj("current" -> (routes.Graph.getPointByTypeOrIdentifier(accId).absoluteURL() + "?type=%s&limit=%d&offset=%d".format(typeString, limit, offset)))
          if(total > offset+limit){
            result ++= Json.obj("next" -> (routes.Graph.getPointByTypeOrIdentifier(accId).absoluteURL() + "?type=%s&limit=%d&offset=%d".format(typeString, limit, offset+limit)))
          }
          if( offset > 0 && (offset < limit || offset-limit >= 0)) {
            result ++= Json.obj("previous" -> (routes.Graph.getPointByTypeOrIdentifier(accId).absoluteURL() + "?type=%s&limit=%d&offset=%d".format(typeString, limit, math.max(0, offset-limit))))
          }
          request.queryString.get("callback").flatMap(_.headOption) match {
            case Some(callback) => Ok(Jsonp(callback, result))
            case None => Ok(result)
          }
        }
      }else if(identifier != ""){
        val list: List[Point] = Point.findAllByIdentifier(accId, identifier, limit, offset)
        val total = Point.countByIdentifier(accId, identifier)

        if(list.length == 0){
          request.queryString.get("callback").flatMap(_.headOption) match {
            case Some(callback) => Ok(Jsonp(callback, Application.JsonStatus(404, "Point not found. Out of %d.".format(total))))
            case None => Application.NotFoundJson(404, "Point not found. Out of %d.".format(total))
          }
        }else{
          var array: JsArray = new JsArray()
          list.foreach { point: Point =>
            var _id: Long = point.id.get
            var ptn: String = null

            PointType.findOneById(point.typeId).map { pt =>
              ptn = pt.name
            }.getOrElse {
              throw new Exception("point(typeId=%1$d) isn't supported.".format(point.typeId))
            }

            array +:= Json.obj(
              "id" -> _id,
              "type" -> ptn,
              "identifier" -> point.identifier,
              "createdAt" -> point.createdAt,
              "updatedAt" -> point.updatedAt,
              "referencedAt" -> point.referencedAt,
              "data" -> point.data
            )
          }
          

          var result: JsObject = Json.obj(
            "status" -> Json.obj(
                "code" -> 200,
                "message" -> ""
              ),
            "points" -> array,
            "_length" -> list.length,
            "length" -> list.length,
            "size" -> total
          )

          result ++= Json.obj("url" -> (routes.Graph.getPointByTypeOrIdentifier(accId).absoluteURL() + "?identifier=%s".format(identifier)))
          result ++= Json.obj("current" -> (routes.Graph.getPointByTypeOrIdentifier(accId).absoluteURL() + "?identifier=%s&limit=%d&offset=%d".format(identifier, limit, offset)))
          if(total > offset+limit){
            result ++= Json.obj("next" -> (routes.Graph.getPointByTypeOrIdentifier(accId).absoluteURL() + "?identifier=%s&limit=%d&offset=%d".format(identifier, limit, offset+limit)))
          }
          if( offset > 0 && (offset < limit || offset-limit >= 0)) {
            result ++= Json.obj("previous" -> (routes.Graph.getPointByTypeOrIdentifier(accId).absoluteURL() + "?identifier=%s&limit=%d&offset=%d".format(identifier, limit, math.max(0, offset-limit))))
          }

          request.queryString.get("callback").flatMap(_.headOption) match {
            case Some(callback) => Ok(Jsonp(callback, result))
            case None => Ok(result)
          }
        }
      }else{
        throw new Exception("point(identifier=?, type=?) how can I do for you? ") 
      }
    } catch { 
      case e: Exception =>
        e.printStackTrace()
        val json = Json.obj(
          "status" -> Json.obj(
            "code" -> 400,
            "message" -> {
              e.getMessage()
            }
          )
        )
        request.queryString.get("callback").flatMap(_.headOption) match {
          case Some(callback) => Ok(Jsonp(callback, json))
          case None => BadRequest(json)
        }
    }
  }

  /*
  {
    "edge": {
      "subjectId": 1,
      "verb": "read",
      "objectId": 2
    }
  }
   */
  def linkWithEdge(accId: Long) = SignedAPI(accId) { implicit request =>
    try {
      var content = request.body.asJson
      request.queryString.get("json").flatMap(_.headOption) match {
        case Some(json) => {
          content = Some(Json.parse(json))
        }
        case None => {}
      }
      content.map { json =>
        (json \ "edge").asOpt[JsObject].map { e =>
          // subjectId => subjectIdentifier
          // objectId => objectIdentifier
          val edgeRead = (
            (__ \ "verb").read[String] ~
            (__ \ "subjectId").read[String] ~
            (__ \ "objectId").read[String] ~
            (__ \ "subjectType").read[String] ~
            (__ \ "objectType").read[String]
          ) tupled

          edgeRead.reads(e).fold(
            valid = { edgeContent =>              
              var _sId = edgeContent._2
              var _oId = edgeContent._3

              var v = edgeContent._1
              var sId = 0L
              var oId = 0L
              var sTypeId = 0L
              var oTypeId = 0L

              if(v.length < 3){
                throw new Exception("point(type=?, id=?) '?' point(type=?, id=?): verb must have at least 3 characters.")
              }
              if(v.length > 20){
                throw new Exception("point(type=?, id=?) '?' point(type=?, id=?): verb must have less than or 20 characters.")
              }
              
              Point.findOneByTypeNameAndIdentifier(accId, edgeContent._4, edgeContent._2).map { subjectPoint =>
                sId = subjectPoint.id.get
                sTypeId = subjectPoint.typeId
              }.getOrElse {
                throw new Exception("unknown point(type=%1$s, identifier=%2$s) '%3$s' point(type=%4$s, identifier=%5$s): subject point isn't found.".format(edgeContent._4, edgeContent._2, v, edgeContent._5, edgeContent._3))
              }

              Point.findOneByTypeNameAndIdentifier(accId, edgeContent._5, edgeContent._3).map { objectPoint =>
                oId = objectPoint.id.get
                oTypeId = objectPoint.typeId
              }.getOrElse {
                throw new Exception("point(type=%1$s, identifier=%2$s) '%3$s' unknown point(type=%4$s, identifier=%5$s): object point isn't found.".format(edgeContent._4, edgeContent._2, v, edgeContent._5, edgeContent._3))
              }

              // if(sTypeId == oTypeId){
              // references between same type, identifier are okay
              if(sId == oId){
                throw new Exception("point(type=%1$s, identifier=%2$s) '%3$s' point(type=%4$s, identifier=%5$s): no self-reference and iteratable relationship are allowed.".format(edgeContent._4, edgeContent._2, v, edgeContent._5, edgeContent._3))
              }
              val edge = Edge(accId, sId, sTypeId, v, oId, oTypeId)
              Edge.add( edge ) map { id: Long =>
                import java.util.Date
                val now = new Date
                Point.referenced(sId, now)
                Point.referenced(oId, now)
                
                val json = Json.obj(
                  "status" -> Json.obj(
                    "code" -> 201,
                    "message" -> "Edge created."
                  )
                )
                request.queryString.get("callback").flatMap(_.headOption) match {
                  case Some(callback) => Created(Jsonp(callback, json))
                  case None => Created(json)
                }
              } getOrElse {
                val json = Json.obj(
                  "status" -> Json.obj(
                    "code" -> 500,
                    "message" -> {
                      "Edge not created. Try again."
                    }
                  )
                )
                request.queryString.get("callback").flatMap(_.headOption) match {
                  case Some(callback) => Ok(Jsonp(callback, json))
                  case None => InternalServerError(json)
                }
              }
            },
            invalid = { error =>
              throw new Exception("point(type=?, id=?) %1$s is undefined. ".format(error.head._1.toJsonString.split('.')(1)))
            }
          )
        } getOrElse {
          throw new Exception("point(type=?, id=?) '...' point(type=?, id=?): no points are selected.")
        }
      } getOrElse {
        throw new Exception("point(type=?, id=?) '...' point(type=?, id=?): no points are selected.")
      }
    } catch { 
      case e: Exception =>
        e.printStackTrace()
        val json = Json.obj(
          "status" -> Json.obj(
            "code" -> 400,
            "message" -> {
              e.getMessage()
            }
          )
        )
        request.queryString.get("callback").flatMap(_.headOption) match {
          case Some(callback) => Ok(Jsonp(callback, json))
          case None => BadRequest(json)
        }
    }
  }
}