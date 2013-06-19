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
import scala.collection.mutable.LinkedHashMap

object Edges extends Controller with Secured {

  /*
    /v1/account/1/edge?subjectId=1&subjectType=user&verb=read&objectId=0&objectType=post
   */
  def find( accId: Long,
    subjectId: Long = -1, subjectType: String = "", subjectIdentifier: String = "",
    verb: String = "",
    objectId: Long = -1L, objectType: String = "", objectIdentifier: String = "",
    getInnerPoints: Boolean = false, limit: Int = 100, offset: Int = 0) = SignedAPI(accId) { implicit request =>
    try {
      var sId: Long     = subjectId
      var sType         = subjectType
      var sTypeId       = -1L
      var sIdentifier   = subjectIdentifier
      var v             = verb
      var oId           = objectId
      var oType         = objectType
      var oTypeId       = -1L
      var oIdentifier   = objectIdentifier

      var complexity = 0.0
      var audit: List[(String, Long)] = List()
      // check complexity
      if(v == ""){
        complexity += 3
        audit = ("no verb = 3", 3L) +: audit
      }
      var sDefined = true
      var oDefined = true

      var param = List[(String, Any)]()

      if(limit < 1){
        throw new Exception("edge(?): limit must be equal or bigger than 1.")
      }

      if(offset < 0){
        throw new Exception("edge(?): offset must start from 0.")
      }

      if(v.length == 1 || v.length == 2){
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
          case _ => throw new Exception("unknown point(id=%1$s): subject point isn't found.".format(sId))
        }
      }

      if(oId != -1){
        Point.getTypeId(accId, oId) match {
          case Some(id: Long) => {
            oTypeId = id
          }
          case _ => throw new Exception("unknown point(id=%1$s): object point isn't found.".format(oId))
        }
      }

      if(sType.length > 0){
        PointType.findOneByName(sType).map { pt =>
          sTypeId = pt.id.get
          if(sIdentifier.length > 0){
            Point.findOneByTypeIdAndIdentifier(accId, sTypeId, sIdentifier).map { point =>
              sId = point.id.get
            }.getOrElse {
              throw new Exception("edge(?): subject identifier of '%1$s' cannot be found.".format(sIdentifier))
            }
          }
        }.getOrElse {
          throw new Exception("edge(?): subject type of '%1$s' isn't supported.".format(sType))
        }
      }

      if(oType.length > 0){
        PointType.findOneByName(oType).map { pt =>
          oTypeId = pt.id.get
          if(oIdentifier.length > 0){
            Point.findOneByTypeIdAndIdentifier(accId, oTypeId, oIdentifier).map { point =>
              oId = point.id.get
            }.getOrElse {
              throw new Exception("edge(?): object identifier of '%1$s' cannot be found.".format(oIdentifier))
            }
          }
        }.getOrElse {
          throw new Exception("edge(?): object type of '%1$s' isn't supported.".format(oType))
        }
      }

      if(sId == -1){
        if(sType.length == 0){
          if(sIdentifier.length == 0){
            complexity += 2 
            sDefined = false
            // audit = ("no subject id & identifier & type = 2", 2L) +: audit
          }else{
            complexity += 0.7 
            // audit = ("no subject id & type but identifier = 0.7", 2L) +: audit
          }
        }else{
          if(sIdentifier.length == 0){
            complexity += 1
            // audit = ("no subject id & identifier but type is = 1", 1L) +: audit
          }else{
            // sDefined = true
            // type + identifier = id
          }
        }
      }
      if(oId == -1){
        if(oType.length == 0){
          if(oIdentifier.length == 0){
            complexity += 2 
            oDefined = false
            // audit = ("no object id & identifier & type = 2", 2L) +: audit
          }else{
            complexity += 0.7
            // audit = ("no object id & type but identifier = 0.7", 2L) +: audit
          }
        }else{
          if(oIdentifier.length == 0){
            complexity += 1
            // audit = ("no object id but type is = 1", 1L) +: audit
          }else{
            // oDefined = true
            // type + identifier = id
          }
        }
      }

      //audit = (sDefined + " || " + oDefined, 0L) +: audit
      if((sDefined || oDefined) == false){
        complexity += 2
        // audit = ("no models = 2", 2L) +: audit
      }

      // discount complexity when limit is given
      if(limit <= 100){
        complexity -= 0.5
      }

      // val sum = audit.foldLeft(("Audit logs: \n", 0L)){ (a: (String, Long), b: (String, Long)) =>
      //   (a._1 + "\t" + b._1 + "\n", a._2 + b._2)
      // }
      // println("Comp = " + sum._2 + "\n" + sum._1)

      val maxComplexity = 9
      /*
       * sId + v + oId = 0
       * sType + v + oType = 2
       * sId + v = 2
       * sId + oId = 3
       * sId = 5
       * sType + oType = 5 (4.95 with limit)
       * v = 6
       */
      if(complexity >= 5){
        val uuid = java.util.UUID.randomUUID().toString
        val p = com.mintpresso.Point(0, "error", uuid, Json.obj(
          "message" -> "complexity limit reached. %f >= 5.000".format(complexity / maxComplexity),
          "complexity" -> (complexity / maxComplexity),
          "url" -> request.uri
        ).toString, "", 0, 0, 0)

        val _identifier = mintpresso.get(accId).get.identifier
        mintpresso.set(p) match {
          case Some(point) => mintpresso.set("user", _identifier, "log", "error", uuid)
          case None => Logger.info("Not logged. Account("+_identifier+") uri("+request.uri+") complexity limit reached. %f >= 5.000".format(complexity / maxComplexity))
        }
        throw new Exception("edge(?): the pseudo edge specified in query has too many unknown fields. Calculated complexity is %f".format(complexity / maxComplexity))
      }

      // prepare variables and arguments
      
      // find cache

      // generate new query for search
      var conditions: Map[String, String] = Map()
      var additional: LinkedHashMap[String, String] = LinkedHashMap()

      // println("QUERY: %s / %s".format(sId, sTypeId))
      if(sId != -1){
        conditions += ("sId" -> sId.toString, "sType" -> sTypeId.toString)
      }else if(sTypeId != -1){
        conditions += ("sType" -> sTypeId.toString)
      }
      if(oId != -1){
        conditions += ("oId" -> oId.toString, "oType" -> oTypeId.toString)
      }else if(oTypeId != -1){
        conditions += ("oType" -> oTypeId.toString)
      }

      if(v.length > 0){
        conditions += ("v" -> v)
      }

      // order by `id`
      // additional += ("order by" -> "`id` asc")
      
      // limit and offset
      additional += ("limit" -> "%d, %d".format(offset, limit))

      val list: List[Edge] = Edge.find(accId, conditions, additional)
      val total = Edge.count(accId, conditions)

      if(list.length == 0){
        request.queryString.get("callback").flatMap(_.headOption) match {
          case Some(callback) => Ok(Jsonp(callback, Application.JsonStatus(404, "Edge not found. Out of %d.".format(total))))
          case None => Application.NotFoundJson(404, "Edge not found. Out of %d.".format(total))
        }
      }else{
        var array: JsArray = new JsArray()
        var subjectType: String = null
        var objectType: String = null

        list.foreach { edge: Edge =>
          /*
            // Other strategy for find a PointType

            val pointTypes: List[PointType] = PointType.findAll // or Cached List[PointType]
            if(pointTypes.filter(_.id.get==edge.sType).length != 0) {
              subjectType = pt.name
            } else {
              throw new Exception
            }

          */ 
          PointType.findOneById(edge.sType).map { pt =>
            subjectType = pt.name
          }.getOrElse {
            throw new Exception("edge(sId=%1$s): subject type of '%2$s' isn't supported.".format(edge.sId, edge.sType))
          }

          PointType.findOneById(edge.oType).map { pt =>
            objectType = pt.name
          }.getOrElse {
            throw new Exception("edge(oId=%1$s): subject type of '%2$s' isn't supported.".format(edge.oId, edge.oType))
          }
          
          if(getInnerPoints == true){
            (Point.findOneById(accId, edge.sId), Point.findOneById(accId, edge.oId)) match {
              case (Some(sModel), Some(oModel)) => {
                // both points found
                (PointType.findOneById(sModel.typeId), PointType.findOneById(oModel.typeId)) match {
                  // both types found
                  case (Some(_sType), Some(_oType)) => {
                    val sModelType = _sType.name
                    val oModelType = _oType.name
                    array +:= Json.obj(
                      "subjectId" -> edge.sId,
                      "subjectType" -> subjectType,
                      "subject" -> Json.obj(
                        "id" -> sModel.id.get.toLong,
                        "type" -> sModelType,
                        "identifier" -> sModel.identifier,
                        "createdAt" -> sModel.createdAt,
                        "updatedAt" -> sModel.updatedAt,
                        "referencedAt" -> sModel.referencedAt,
                        "data" -> sModel.data,
                        "_url" -> routes.Graph.getPoint(accId, sModel.id.get.toLong).absoluteURL()
                      ),
                      "verb" -> edge.v,
                      "objectId" -> edge.oId,
                      "objectType" -> objectType,
                      "object" -> Json.obj(
                        "id" -> oModel.id.get.toLong,
                        "type" -> oModelType,
                        "identifier" -> oModel.identifier,
                        "createdAt" -> oModel.createdAt,
                        "updatedAt" -> oModel.updatedAt,
                        "referencedAt" -> oModel.referencedAt,
                        "data" -> oModel.data,
                        "_url" -> routes.Graph.getPoint(accId, oModel.id.get.toLong).absoluteURL()
                      ),
                      "createdAt" -> edge.createdAt,
                      "_url" -> (routes.Edges.find(accId, subjectId, subjectType, subjectIdentifier, verb, objectId, objectType, objectIdentifier).absoluteURL())
                    )
                  }
                  case (None, Some(_)) => {
                    throw new Exception("edge(sId=%1$s, oId=%1$s): subject type of '%3$s' isn't supported.".format(edge.sId, edge.oId, edge.sType))
                  }
                  case (Some(_), None) => {
                    throw new Exception("edge(sId=%1$s, oId=%1$s): object type of '%3$s' isn't supported.".format(edge.sId, edge.oId, edge.oType))
                  }
                  case (_, _) => {
                    throw new Exception("edge(sId=%1$s, oId=%1$s): subject and object types of {%3$s, %4$s} isn't supported.".format(edge.sId, edge.oId, edge.sType, edge.oType))
                  }
                }
              }
              case (_, _) => {
                // one or both not found, ignore innerModels
                array +:= Json.obj(
                  "subjectId" -> edge.sId,
                  "subjectType" -> subjectType,
                  "verb" -> edge.v,
                  "objectId" -> edge.oId,
                  "objectType" -> objectType,
                  "createdAt" -> edge.createdAt
                )
              }
            }
          }else{
            array +:= Json.obj(
              "subjectId" -> edge.sId,
              "subjectType" -> subjectType,
              "verb" -> edge.v,
              "objectId" -> edge.oId,
              "objectType" -> objectType,
              "createdAt" -> edge.createdAt
            )
          }
        }

        // _length become an legacy support from 2013/6/18
        var result: JsObject = Json.obj(
          "status" -> Json.obj(
              "code" -> 200,
              "message" -> ""
            ),
          "edges" -> array,
          "length" -> list.length,
          "_length" -> list.length,
          "size" -> total
          )

        result ++= Json.obj("url" -> (routes.Edges.find(accId, subjectId, subjectType, subjectIdentifier, verb, objectId, objectType, objectIdentifier, getInnerPoints).absoluteURL()))
        result ++= Json.obj("current" -> (routes.Edges.find(accId, subjectId, subjectType, subjectIdentifier, verb, objectId, objectType, objectIdentifier, getInnerPoints, limit, offset).absoluteURL()))
        if(total > offset+limit){
          result ++= Json.obj("next" -> (routes.Edges.find(accId, subjectId, subjectType, subjectIdentifier, verb, objectId, objectType, objectIdentifier, getInnerPoints, limit, offset+limit).absoluteURL()))
        }
        if( offset > 0 && (offset < limit || offset-limit >= 0)) {
          result ++= Json.obj("previous" -> (routes.Edges.find(accId, subjectId, subjectType, subjectIdentifier, verb, objectId, objectType, objectIdentifier, getInnerPoints, limit, math.max(0, offset-limit)).absoluteURL()))
        }

        request.queryString.get("callback").flatMap(_.headOption) match {
          case Some(callback) => Ok(Jsonp(callback, result))
          case None => Ok(result)
        }
      }
    } catch { 
      case e: Exception =>
        e.printStackTrace()
        val json = Json.obj(
          "status" -> Json.obj(
            "code" -> 400,
            "message" -> e.getMessage()
          )
        )
        request.queryString.get("callback").flatMap(_.headOption) match {
          case Some(callback) => Ok(Jsonp(callback, json))
          case None => BadRequest(json)
        }
    }
  }
}