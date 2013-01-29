package models

import play.api.Play.current
import play.api.db._
import anorm._ 
import anorm.SqlParser._
import java.util.Date
import play.api.libs.json._
import play.api.libs.json.Json._

case class Point(var id: Pk[Long], accountId: Long, typeId: Long, identifier: String, createdAt: Date, updatedAt: Date, referencedAt: Date, data: JsObject)

object Point {
  val parser = {
    get[Pk[Long]]("id")~
    get[Long]("accountId")~
    get[Long]("typeId")~ 
    get[String]("identifier")~ 
    get[Date]("createdAt")~ 
    get[Date]("updatedAt")~ 
    get[Date]("referencedAt")~ 
    get[String]("data") map {
      case pk~l1~l2~s1~d1~d2~d3~s2 => {
        new Point(pk, l1, l2, s1, d1, d2, d3, Json.obj( "data" -> Json.parse(s2)))
      }
    }
  }
  val Type: Map[String, Long] = Map(
      "user" -> 10,
      "page" -> 20,
      "post" -> 30
    )
  val TypeString: Map[Long, String] = Map(
      10L -> "user",
      20L -> "page",
      30L -> "post"
    )
  def apply(accId: Long, typeString: String, identifier: String, data: JsObject): Point = {
    var typeId: Long = -1
    Point.Type.get(typeString) match {
      case id: Some[Long] => typeId = id.get
      case None => throw new Exception("point(identifier=" + identifier + ") type: '" + typeString + "' isn't supported.")
    }

    if(identifier.length > 0){
      this.findOneByTypeIdAndIdentifier(accId, typeId, identifier) match {
        case row: Some[Point] => {
          row.get
        }
        case _ => {
          new Point(anorm.NotAssigned, accId, typeId, identifier, new Date, new Date, new Date, data )
        }
      }
    }else{
      new Point(anorm.NotAssigned, accId, typeId, identifier, new Date, new Date, new Date, data )
    }
  }

  def findOneById(accId: Long, id: Long): Option[Point] = {
    DB.withConnection { implicit conn =>
      SQL(
        """
          select * from points where id = {id} and accountId = {accId}
        """
      ).on( 'id -> id,
            'accId -> accId
      ).singleOpt(parser)
    }
  }
  
  def findAllByTypeId(accId: Long, typeId: Long, limit: Int, offset: Int): List[Point] = {
    DB.withConnection { implicit conn =>
      SQL(
        """
          select * from points where accountId = {accId} and typeId = {typeId} limit {offset}, {limit}
        """
      ).on( 'accId -> accId, 
            'typeId -> typeId,
            'limit -> limit,
            'offset -> offset
      ).as(parser *)
    }
  }

  def findAllByIdentifier(accId: Long, identifier: String, limit: Int, offset: Int): List[Point] = {
    DB.withConnection { implicit conn =>
      SQL(
        """
          select * from points where accountId = {accId} and identifier = {identifier} limit {offset}, {limit}
        """
      ).on( 'accId -> accId, 
            'identifier -> identifier,
            'limit -> limit,
            'offset -> offset
      ).as(parser *)
    }
  }

  def findOneByTypeIdAndIdentifier(accId: Long, typeId: Long, identifier: String): Option[Point] = {
    DB.withConnection { implicit conn =>
      SQL(
        """
          select * from points where accountId = {accId} and typeId = {typeId} and identifier = {identifier}
        """
      ).on( 'accId -> accId, 
            'typeId -> typeId, 
            'identifier -> identifier
      ).singleOpt(parser)
    }
  }

  def findAllByLatest(accId: Long): List[Point] = {
    DB.withConnection { implicit conn =>
      SQL(
        """
          select * from points where accountId = {accId} order by createdAt desc limit 0, 10
        """
      ).on( 'accId -> accId
      ).as(parser *)
    }  
  }

  def verifyPoints(accId: Long, pIdA: Long, pIdB: Long): Boolean = {
    DB.withConnection { implicit conn =>
      val result: Int = SQL(
        """
          select count(id) from points where id = {a} or {b}
        """
      ).on( 'a -> pIdA,
            'b -> pIdB
      ).as(scalar[Int].single)
      if(result == 2){
        true
      }else{
        false
      }
    }
  }

  def getTypeId(accId: Long, pId: Long): Option[Long] = {
    DB.withConnection { implicit conn =>
      SQL(
        """
          select typeId from points where id = {id}
        """
      ).on( 'id -> pId ).as(scalar[Long].singleOpt)
    }
  }

  def getTypes(accId: Long): List[String] = {
    DB.withConnection { implicit conn =>
      val list: List[Int] = SQL(
        """
          select distinct typeId as type from points where accountId = {id}
        """
      ).on( 'id -> accId ).as(
        int("type") *
      )
      var res: List[String] = List()
      list.foreach { i =>
        res = res :+ TypeString(i).toString
      }
      res
    }
  }

  def add(point: Point):Option[Long] = {
    DB.withConnection { implicit conn =>
      val id: Option[Long] = SQL(
        """
        insert into points (typeId, accountId, identifier, createdAt, updatedAt, referencedAt, data)
        values ({typeId}, {accountId}, {identifier}, {createdAt}, {updatedAt}, {referencedAt}, {data})
        """
      ).on(
        'typeId         -> point.typeId, 
        'accountId      -> point.accountId, 
        'identifier     -> point.identifier, 
        'createdAt      -> point.createdAt, 
        'updatedAt      -> point.updatedAt, 
        'referencedAt   -> point.referencedAt, 
        'data           -> Json.stringify(point.data)
      ).executeInsert()
      id
    }
  }
}
