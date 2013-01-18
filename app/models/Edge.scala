package models

import play.api.Play.current
import play.api.db._
import anorm._ 
import anorm.SqlParser._
import java.util.Date
import play.api.libs.json._
import play.api.libs.json.Json._

case class Edge(id: Pk[Any], sId: Long, sType: Long, v: String, oId: Long, oType: Long, createdAt: Date){

}

object Edge {
  val parser = {
    get[Pk[Long]]("id")~
    get[Long]("sId")~
    get[Long]("sType")~ 
    get[String]("v")~ 
    get[Long]("oId")~
    get[Long]("oType")~ 
    get[Date]("createdAt") map {
      case pk~l1~l2~s1~l3~l4~d1 => {
        new Edge(pk, l1, l2, s1, l3, l4, d1)
      }
    }
  }

  def apply(sId: Long, sTypeId: Long, v: String, oId: Long, oTypeId: Long): Edge = {
    new Edge(anorm.NotAssigned, sId, sTypeId, v, oId, oTypeId, new Date())
  }

  def add(edge: Edge): Option[Long] = {
    DB.withConnection { implicit conn =>
      SQL(
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
      Some(0L)
    }
  }
}
