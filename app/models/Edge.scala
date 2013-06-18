package models

import play.api.Play.current
import play.api.db._
import play.api.Logger
import anorm._ 
import anorm.SqlParser._
import java.util.Date
import play.api.libs.json._
import play.api.libs.json.Json._
import scala.collection.mutable.LinkedHashMap

case class Edge(id: Pk[Any], accountId: Long, sId: Long, sType: Long, v: String, oId: Long, oType: Long, createdAt: Date){

}
object MetaQueryBuilder { 
  import scala.language.implicitConversions
  implicit def tToStringMap[T](m: Map[T, String]): Map[String, String] = m.map { a =>
    a._1 match {
      case x: String => (x -> a._2)
      case x: Symbol => (x.name -> a._2)
      case x => throw new Exception(x.getClass.toString() + " is invalid type.")
    }
  }

  def apply[T](query: String, where: Map[T, String], additional: Map[T, String] = Map[T, String]()): SimpleSql[Row] = {
    val stringWhere: Map[String, String] = where
    val stringAddition: Map[String, String] = additional
    var d = Seq[(Any, anorm.ParameterValue[_])]()
    var qry = Seq[String]()
    var additionalQry = Seq[String]()
    for((k, v) <- stringWhere) {
      val tmp = (k -> v): (Any, anorm.ParameterValue[_])
      d = d :+ tmp
      qry = qry :+ "%1$s = {%1$s}".format(k)
    }

    for((k, v) <- stringAddition) {
      additionalQry = additionalQry :+ "%s %s".format(k, v)
    }
    val condition = qry.reduceLeft("%s AND %s".format(_, _))
    var additionalOption = ""
    if(additionalQry.length > 0) {
     additionalOption = additionalQry.reduce("%s %s".format(_, _))
    }
    SQL(s"$query WHERE $condition $additionalOption").on(d: _*)
  }
}


object Edge {
  val parser = {
    get[Pk[Long]]("id")~
    get[Long]("sId")~
    get[Long]("sType")~ 
    get[String]("v")~ 
    get[Long]("oId")~
    get[Long]("oType")~ 
    get[Date]("createdAt")~
    get[Long]("accountId") map {
      case pk~l1~l2~s1~l3~l4~d1~l5 => {
        new Edge(pk, l5, l1, l2, s1, l3, l4, d1)
      }
    }
  }

  def apply(accountId: Long, sId: Long, sTypeId: Long, v: String, oId: Long, oTypeId: Long): Edge = {
    new Edge(anorm.NotAssigned, accountId, sId, sTypeId, v, oId, oTypeId, new Date())
  }
 
  def find(accountId: Long, conditions: Map[String, String], additional: LinkedHashMap[String, String]): List[Edge] = {
    DB.withConnection { implicit conn =>
      val where = Map("accountId" -> accountId.toString) ++ conditions
      val query = "SELECT * FROM `edges`"
      MetaQueryBuilder(query, where, additional.toMap).as(parser *)
    }
  }
    }
  }

  def add(edge: Edge): Option[Long] = {
    DB.withConnection { implicit conn =>
      SQL(
        """
        insert into edges (sId, sType, v, oId, oType, createdAt, accountId)
        values ({sId}, {sType}, {v}, {oId}, {oType}, {createdAt}, {accountId})
        """
      ).on(
        'sId -> edge.sId,
        'sType -> edge.sType,
        'v -> edge.v,
        'oId -> edge.oId,
        'oType -> edge.oType,
        'createdAt -> edge.createdAt,
        'accountId -> edge.accountId
      ).executeInsert()
      Some(0L)
    }
  }
}
