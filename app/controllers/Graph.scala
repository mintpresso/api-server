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
      content.map { json =>
        (json \ "point").asOpt[JsObject].map { obj =>
          var code: Int = 0
          var msg: String = ""

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
            case None => {
              msg += "json.invalid"
              Json.obj()
            }
          }

          var point: Point = Point(accId, typeString.get, _identifier, _data)
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
              code = 200
              msg += "Already defined. "
            }
          }

          val result: JsObject = Json.obj(
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
        throw new Exception("""Json object 'point' is required like this: { "point": ... } """)
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
          case Some(callback) => BadRequest(Jsonp(callback, json))
          case None => BadRequest(json)
        }
    }
  }

  def getPoint(accId: Long, id: Long) = SignedAPI(accId) { implicit request =>
    Point.findOneById(accId, id) map { point: Point =>
      var _id: Long = point.id.get
    
      val json = Json.obj(
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
      )
      request.queryString.get("callback").flatMap(_.headOption) match {
        case Some(callback) => Ok(Jsonp(callback, json))
        case None => Ok(json)
      }
    } getOrElse {
      request.queryString.get("callback").flatMap(_.headOption) match {
        case Some(callback) => Ok(Jsonp(callback, Application.JsonStatus(404)))
        case None => Application.NotFoundJson()
      }
    }
  }

  def getPointTypes(accId: Long) = SignedAPI(accId) { implicit request =>
    val str = Point.getTypes(accId) collect { case s: String => "\"%s\"".format(s) } mkString(", ")
    Ok(Json.parse("[" + str + "]"))
  }

  def getPointLatest(accId: Long) = SignedAPI(accId) { implicit request =>
    val list: List[Point] = Point.findAllByLatest(accId)
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
      var result: JsObject = Json.obj(
        "status" -> Json.obj(
            "code" -> 200,
            "message" -> ""
          ),
        "points" -> array
        )
      Ok(result)
    }
  }
  def getPointByTypeOrIdentifier(accId: Long) = SignedAPI(accId) { implicit request =>
    var typeString: String = request.queryString.get("type").flatMap(_.headOption).getOrElse("").toString
    var identifier: String = request.queryString.get("identifier").flatMap(_.headOption).getOrElse("").toString
    val Number = "([0-9]+)".r
    var limit: Int = 10
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
        var typeId: Long = -1
        Point.Type.get(typeString) match {
          case Some(id: Long) => typeId = id
          case _ => throw new Exception("point(identifier=%1$s, type=%2$s) type: '%2$s' isn't supported.".format(identifier, typeString))
        }
        Point.findOneByTypeIdAndIdentifier(accId, typeId, identifier) map { point: Point =>
          var _id: Long = point.id.get
          val json = Json.obj(
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
        Point.Type.get(typeString) match {
          case Some(id: Long) => typeId = id
          case _ => throw new Exception("point(identifier=?, type=%1$s) type: '%1$s' isn't supported.".format(typeString))
        }
        val list: List[Point] = Point.findAllByTypeId(accId, typeId, limit, offset)
        if(list.length == 0){
          request.queryString.get("callback").flatMap(_.headOption) match {
            case Some(callback) => Ok(Jsonp(callback, Application.JsonStatus(404, "Point not found")))
            case None => Application.NotFoundJson(404, "Point not found")
          }
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
          request.queryString.get("callback").flatMap(_.headOption) match {
            case Some(callback) => Ok(Jsonp(callback, result))
            case None => Ok(result)
          }
        }
      }else if(identifier != ""){
        val list: List[Point] = Point.findAllByIdentifier(accId, identifier, limit, offset)
        if(list.length == 0){
          request.queryString.get("callback").flatMap(_.headOption) match {
            case Some(callback) => Ok(Jsonp(callback, Application.JsonStatus(404, "Point not found")))
            case None => Application.NotFoundJson(404, "Point not found")
          }
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
        val json = Json.obj(
          "status" -> Json.obj(
            "code" -> 400,
            "message" -> {
              e.getMessage()
            }
          )
        )
        request.queryString.get("callback").flatMap(_.headOption) match {
          case Some(callback) => BadRequest(Jsonp(callback, json))
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
        case None => 
      }
      content.map { json =>
        (json \ "edge").asOpt[JsObject].map { e =>
          var _sId = (e \ "subjectId").asOpt[Int]
          var _v = (e \ "verb").asOpt[String]
          var _oId = (e \ "objectId").asOpt[Int]

          var sId = 0L
          var v = ""
          var oId = 0L
          var sTypeId: Long = -1
          var oTypeId: Long = -1

          _v match {
            case Some(verb: String) => {
              if(verb.length < 3){
                throw new Exception("point(type=?, id=?) '?' point(type=?, id=?): verb must have at least 3 characters.")
              }
              if(verb.length > 20){
                throw new Exception("point(type=?, id=?) '?' point(type=?, id=?): verb must have less than or 20 characters.")
              }
              v = verb
            }
            case _ => throw new Exception("point(type,id) '?' point(type,id): no verb is described.")
          }
          

          (_sId, _oId) match {
            case (Some(x: Int), Some(y: Int)) => {
              sId = x
              oId = y
              // optimization code (shard index) comes here.
              Point.getTypeId(accId, x) match {
                case Some(id: Long) => {
                  sTypeId = id
                }
                case _ => {
                  throw new Exception("unknown point(id=%1$s) '%3$s' point(id=%2$s): subject point isn't found.".format(x, y, v))
                }
              }
              Point.getTypeId(accId, y) match {
                case Some(id: Long) => {
                  oTypeId = id
                }
                case _ => {
                  throw new Exception("point(id=%1$s, type=%4$s) '%3$s' unknown point(id=%2$s): object point isn't found.".format(x, y, v, Point.TypeString(sTypeId)))
                }
              }
              if(sTypeId == oTypeId){
                throw new Exception("point(id=%1$s, type=%2$s) '%3$s' point(id=%1$s, type=%2$s): no self-reference and iteratable relationship are allowed.".format(x, Point.TypeString(sTypeId), v))
              }
            }
            case _ => throw new Exception("point(type=?, id=%1$s) '%3$s' point(type=?, id=%2$s): id of point must be a number.".format(sId, oId, v))
          }
          val edge = Edge(sId, sTypeId, v, oId, oTypeId)
          Edge.add( edge ) map { id: Long =>
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
              case Some(callback) => InternalServerError(Jsonp(callback, json))
              case None => InternalServerError(json)
            }
          }
        } getOrElse {
          throw new Exception("point(type=?, id=?) '...' point(type=?, id=?): no points are selected.")
        }
      } getOrElse {
        throw new Exception("point(type=?, id=?) '...' point(type=?, id=?): no points are selected.")
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
          case Some(callback) => BadRequest(Jsonp(callback, json))
          case None => BadRequest(json)
        }
    }
  }

  /*
    /v1/account/1/edge?subjectId=1&subjectType=user&verb=read&objectId=0&objectType=post
   */
  def findEdges(accId: Long) = SignedAPI(accId) { implicit request =>
    try {
      val (sI, sT, vOpt, oI, oT) = Form(
        tuple(
          "subjectId" -> optional(longNumber),
          "subjectType" -> optional(text),
          "verb" -> optional(text),
          "objectId" -> optional(longNumber),
          "objectType" -> optional(text)
        )
      ).bindFromRequest.get

      (sI, sT, vOpt, oI, oT) match {
        case (None, None, None, None, None) => throw new Exception("edge(subjectId | subjectType | verb | objectId | oType): no description for edge is given.")
        case _ => 
      }

      var sId: Long = sI.getOrElse(-1L)
      var sType = sT.getOrElse("")
      var sTypeId = -1L
      var v = vOpt.getOrElse("")
      var oId = oI.getOrElse(-1L)
      var oType = oT.getOrElse("")
      var oTypeId = -1L

      var complexity = 0.0
      var audit: List[(String, Long)] = List()
      // check complexity
      if(v == ""){
        complexity += 3
        audit = ("no verb = 3", 3L) +: audit
      }
      var sDefined = true
      var oDefined = true
      if(sId == -1){
        if(sType.length == 0){
          complexity += 2 
          sDefined = false
          audit = ("no subject id & type = 2", 2L) +: audit
        }else{
          complexity += 1
          audit = ("no subject id but type is = 1", 1L) +: audit
        }
      }
      if(oId == -1){
        if(oType.length == 0){
          complexity += 2 
          oDefined = false
          audit = ("no object id & type = 2", 2L) +: audit
        }else{
          complexity += 1
          audit = ("no object id but type is = 1", 1L) +: audit
        }
      }

      //audit = (sDefined + " || " + oDefined, 0L) +: audit
      if((sDefined || oDefined) == false){
        complexity += 2
        audit = ("no models = 2", 2L) +: audit
      }

      val sum = audit.foldLeft(("Audit logs: \n", 0L)){ (a: (String, Long), b: (String, Long)) =>
        (a._1 + "\t" + b._1 + "\n", a._2 + b._2)
      }
      println("Comp = " + sum._2 + "\n" + sum._1)

      val maxComplexity = 9
      /*
       * sId + v + oId = 0
       * sType + v + oType = 2
       * sId + v = 2
       * sId + oId = 3
       * sId = 5
       * sType + oType = 5
       * v = 6
       */
      if(complexity >= 5){
        throw new Exception("edge(?): the pseudo edge specified in query has too many unknown fields. Calculated complexity is %f".format(complexity / maxComplexity))
      }

      // prepare variables and arguments
      if(v.length < 3){
        throw new Exception("edge(?): verb must have at least 3 characters.")
      }
      if(v.length > 20){
        throw new Exception("edge(?): verb must have less than or 20 characters.")
      }

      if(sId != -1){
        Point.getTypeId(accId, sId) match {
          case Some(id: Long) => {
            sTypeId = id
          }
          case _ => {
            throw new Exception("unknown point(id=%1$s): subject point isn't found.".format(sId))
          }
        }
      }else if(sType.length > 0){
        sTypeId = Point.Type.get(sType).getOrElse {
          throw new Exception("edge(?): subject type of '%1$s' isn't supported.".format(sType))
          -1
        }
      }

      if(oId != -1){
        Point.getTypeId(accId, oId) match {
          case Some(id: Long) => {
            oTypeId = id
          }
          case _ => {
            throw new Exception("unknown point(id=%1$s): object point isn't found.".format(oId))
          }
        }
      }else if(oType.length > 0){
        oTypeId = Point.Type.get(oType).getOrElse {
          throw new Exception("edge(?): object type of '%1$s' isn't supported.".format(oType))
          -1
        }
      }

      // find cache

      // generate new query for search
      var args: List[(String, Long)] = List()
      var param: List[(String, Any)] = List()

      if(sId != -1){
        args = args :+ ("sId", sId) :+ ("sType", sTypeId)
        param = param :+ ("subjectId", sId) :+ ("subjectType", sTypeId)
      }else if(sTypeId != -1){
        args = args :+ ("sType", sTypeId)
        param = param :+ ("subjectType", sTypeId)
      }
      if(v.length > 0){
        param = param :+ ("verb", v)
      }
      if(oId != -1){
        args = args :+ ("oId", oId) :+ ("oType", oTypeId)
        param = param :+ ("objectId", oId) :+ ("objectType", oTypeId)
      }else if(oTypeId != -1){
        args = args :+ ("oType", oTypeId)
        param = param :+ ("objectType", oTypeId)
      }


      val list: List[Edge] = Edge.find(Some(v), args:_*)

      if(list.length == 0){
        Application.NotFoundJson(404, "Edge not found")  
        request.queryString.get("callback").flatMap(_.headOption) match {
          case Some(callback) => Ok(Jsonp(callback, Application.JsonStatus(404, "Edge not found")))
          case None => Application.NotFoundJson(404, "Edge not found")
        }
      }else{
        val url: String = (param.foldLeft(("", 0)){ (a: (String, Any), b: (String, Any)) => 
          var delim = "&"
          if(a._1.length == 0){
            delim = "?"
          }
          ("%s%s%s=%s".format(a._1, delim, b._1, b._2), 0)
        })._1

        var array: JsArray = new JsArray()
        list.foreach { edge: Edge =>
          array = Json.obj(
            "subjectId" -> edge.sId,
            "subjectType" -> Point.TypeString(edge.sType),
            "verb" -> edge.v,
            "objectId" -> edge.oId,
            "objectType" -> Point.TypeString(edge.oType),
            "createdAt" -> edge.createdAt,
            "_url" -> (routes.Graph.findEdges(accId).absoluteURL() + url)
            ) +: array
        }
        var result: JsObject = Json.obj(
          "status" -> Json.obj(
              "code" -> 200,
              "message" -> ""
            ),
          "edges" -> array
          )
        request.queryString.get("callback").flatMap(_.headOption) match {
          case Some(callback) => Ok(Jsonp(callback, result))
          case None => Ok(result)
        }
      }
    } catch { 
      case e: Exception =>
        e.printStackTrace()
        println(">> " + request)
        val json = Json.obj(
          "status" -> Json.obj(
            "code" -> 400,
            "message" -> {
              e.getMessage()
            }
          )
        )
        request.queryString.get("callback").flatMap(_.headOption) match {
          case Some(callback) => BadRequest(Jsonp(callback, json))
          case None => BadRequest(json)
        }
    }
  }
}