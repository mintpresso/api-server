package models

import anorm._ 
import play.api.Play.current
import play.api.db.DB
import java.util.Date
import play.api.libs.json._
import play.api.libs.json.Json._

case class Edge(id: Pk[Any], sId: Long, sType: Long, v: String, oId: Long, oType: Long, createdAt: Date){

}

object Edge {
  def apply(sId: Long, sType: String, v: String, oId: Long, oType: String): Edge = {
    var sTypeId: Long = -1
    var oTypeId: Long = -1
    
    Point.Type.get(sType).get match {
      case id: Long => sTypeId = id
      case _ => throw new Exception("subject type: '" + sType + "' isn't supported.")
    }
    Point.Type.get(oType).get match {
      case id: Long => oTypeId = id
      case _ => throw new Exception("object type: '" + oType + "' isn't supported.")
    }
    new Edge(anorm.NotAssigned, sId, sTypeId, v, oId, oTypeId, new Date())
  }

  def add(edge: Edge) = {
    DB.withConnection { implicit conn =>
      val id: Option[Long] = SQL(
        """
        insert into edges (sId, sType, v, oId, oType, createdAt)
        values ({sId}, {sType}, {v}, {oId}, {oType}, {createdAt})
        """
      ).on(
        'sId -> edge.sId,
        'sType -> edge.sType,
        'v -> edge.v,
        'oId -> edge.oId,
        'oType -> edge.oType,
        'createdAt -> edge.createdAt
      ).executeInsert()
      id
    }
  }
}
