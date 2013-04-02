package com.mintpresso

import play.api._
import play.api.i18n.Messages
import play.api.data.Forms._
import play.api.data._
import play.api.Play.current
import play.api.libs._
import play.api.libs.ws._
import play.api.cache._
import play.api.libs.json._
import play.api.libs.iteratee._
import scala.concurrent.stm._
import scala.concurrent._

object MintpressoAPI {
  var connections: Map[String, Mintpresso] = Map()
  
  /*
    MintpressoAPI("internal")
      requires to configure 'mintpresso.internal.id' and 'mintpresso.internal.api' in applicaion.conf
  */
  def apply(label: String): Mintpresso = {
    val id = Play.configuration.getString("mintpresso." + label + ".id").getOrElse("0").toInt
    val token = Play.configuration.getString("mintpresso." + label + ".api").getOrElse("")
    if(id == 0){
      println(id)
      throw new Exception("mintpresso." + label + ".id isn't configued.")
    }
    if(token == ""){
      println(token)
      throw new Exception("mintpresso." + label + ".api isn't configued.") 
    }
    return apply(label, id, token)
  }

  /*
    MintpressoAPI("user", accountId, "token is here")
      token won't be verified when Panel and API servers are paired with access domain so just leave it an empty for internal use.
  */
  def apply(label: String, accountId: Int, token: String): Mintpresso = {
    // API Token consists of {api token}::{account id}
    val tokens = token.split("::")
    // Set key as LABEL + AccountID combi
    val key = label + accountId
    if(!connections.contains(key)){
      val m: Mintpresso = new Mintpresso(accountId, tokens(0))
      connections += ((key, m))
    }
    connections(key)
  }
}
class Mintpresso(accId: Int, token: String) {
  val server = "http://" + Play.configuration.getString("mintpresso.api.server").getOrElse("127.0.0.1:9001")
  val initial = "Play 2.1 API"
  val versionPrefix = "/" + Play.configuration.getString("mintpresso.api.version").getOrElse("v1")
  val urls: Map[String, String] = Map(
    "getPoint" -> (versionPrefix + "/account/%d/point/%d"),
    "getPointType" -> (versionPrefix + "/account/%d/points/type"),
    "getLatestPoint" -> (versionPrefix + "/account/%d/points/latest"),
    "getPointByTypeOrIdentifier" -> (versionPrefix + "/account/%d/point"),
    "addPoint" -> (versionPrefix + "/account/%d/point"),
    "findEdges" -> (versionPrefix + "/account/%d/edge"),
    "linkWithEdge" -> (versionPrefix + "/account/%d/edge")
  )

  def getPoint(id: Int): Future[Response] = {
    WS.url(server + urls("getPoint").format(accId, id))
      .withHeaders( ("X-Requested-With", initial) )
      .withQueryString( ("api_token", token) )
      .get()
  }
  def getPointTypes(): Future[Response] = {
    WS.url(server + urls("getPointType").format(accId))
      .withHeaders( ("X-Requested-With", initial) )
      .withQueryString( ("api_token", token) )
      .get() 
  }
  def getLatestPoints(): Future[Response] = {
    WS.url(server + urls("getLatestPoint").format(accId))
      .withHeaders( ("X-Requested-With", initial) )
      .withQueryString( ("api_token", token) )
      .get()  
  }
  def findByType(typeString: String, limit: Int = 30, offset: Int = 0): Future[Response] = {
    WS.url(server + urls("getPointByTypeOrIdentifier").format(accId))
      .withHeaders( ("X-Requested-With", initial) )
      .withQueryString(("api_token", token), ("type", typeString), ("limit", limit.toString), ("offset", offset.toString))
      .get()
  }
  def findByIdentifier(identifier: String, limit: Int = 30, offset: Int = 0): Future[Response] = {
    WS.url(server + urls("getPointByTypeOrIdentifier").format(accId))
      .withHeaders( ("X-Requested-With", initial) )
      .withQueryString(("api_token", token), ("identifier", identifier), ("limit", limit.toString), ("offset", offset.toString))
      .get()
  }
  def findByTypeAndIdentifier(typeString: String, identifier: String, limit: Int = 30, offset: Int = 0): Future[Response] = {
    WS.url(server + urls("getPointByTypeOrIdentifier").format(accId))
      .withHeaders( ("X-Requested-With", initial) )
      .withQueryString(("api_token", token), ("type", typeString), ("identifier", identifier), ("limit", limit.toString), ("offset", offset.toString))
      .get()
  }
  def addPoint(typeString: String, identifier: String, json: String) = {
    var p1: String = ""
    var p2: String = ""
    if(identifier.length > 0){
      p1 = "\"identifier\": \"%s\",".format(identifier)
    }
    if(json.length > 0){
      p2 = "\"data\": %s,".format(json)
    }
    val body = 
    """
{
  "point": {
    %s %s
    "type": "%s"
  }
}
    """.format(p1, p2, typeString)
    WS.url(server + urls("addPoint").format(accId))
      .withHeaders( ("Content-Type", "application/json"), ("X-Requested-With", initial) )
      .withQueryString( ("api_token", token) )
      .post[String](body)
  }
  def findRelations(query: Map[String, String]) = {
    val queries = query + (("api_token" -> token))
    WS.url(server + urls("findEdges").format(accId))
      .withHeaders( ("X-Requested-With", initial) )
      .withQueryString(queries.toSeq:_*)
      .get()
  }
  def linkWithEdge(query: Map[String, String]) = {
    val body =
    """
{
  "edge": {
    "subjectId": "%s",
    "subjectType": "%s",
    "verb": "%s",
    "objectId": "%s",
    "objectType": "%s"
  }
}
    """.format(
      query.get("subjectId").getOrElse("\"\""),
      query.get("subjectType").getOrElse("\"\""),
      query.get("verb").getOrElse("\"\""),
      query.get("objectId").getOrElse("\"\""),
      query.get("objectType").getOrElse("\"\"")
    )
    WS.url(server + urls("linkWithEdge").format(accId))
      .withHeaders( ("X-Requested-With", initial) )
      .withQueryString( ("api_token" -> token), ("json" -> body) )
      .post( body )
    // var map: Map[String, Seq[String]] = Map()
    // query.foreach { kv =>
    //   map += (kv._1 -> Seq(kv._2))
    // }
    // WS.url(server + urls("linkWithEdge").format(accId))
    //   .withHeaders( ("X-Requested-With", initial) )
    //   .withQueryString( (("api_token"), token) )
    //   .post( map )
  }
}