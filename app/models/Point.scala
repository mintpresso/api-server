package models

import play.api.Play.current
import play.api.db._
import anorm._ 
import anorm.SqlParser._
import java.util.Date
import play.api.libs.json._
import play.api.libs.json.Json._

case class Point(var id: Pk[Long], accountId: Long, typeId: Long, identifier: String, var createdAt: Date, var updatedAt: Date, var referencedAt: Date, var data: JsValue)

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
        new Point(pk, l1, l2, s1, d1, d2, d3, Json.parse(s2))
      }
    }
  }
  
  def apply(accId: Long, typeString: String, identifier: String, data: JsObject): Point = {
    var typeId: Long = -1
    PointType.findOneByName(typeString) map { pt =>
      typeId = pt.id.get
    } getOrElse {
      PointType.add(PointType(NotAssigned, typeString)) map { id =>
        typeId = id 
      } getOrElse {        
        throw new Exception("point(type=%1$s) cannot be added.")
      }
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

  def findOneByTypeNameAndIdentifier(accId: Long, typeName: String, identifier: String): Option[Point] = {
    DB.withConnection { implicit conn =>
      SQL(
        """
          SELECT p.*
          FROM points p, pointTypes pt
          WHERE p.typeId = pt.id
            AND p.accountId = {accId} 
            AND pt.name = {name}
            AND p.identifier = {identifier}
        """
      ).on( 'accId -> accId, 
            'name -> typeName, 
            'identifier -> identifier
      ).singleOpt(parser)
    }
  }

  def findAllByLatest(accId: Long): List[Point] = {
    DB.withConnection { implicit conn =>
      SQL(
        """
          select * from points where accountId = {accId} order by createdAt desc limit 0, 30
        """
      ).on( 'accId -> accId
      ).as(parser *)
    }  
  }

  def verifyPoints(accId: Long, pIdA: Long, pIdB: Long): Boolean = {
    DB.withConnection { implicit conn =>
      val result: Int = SQL(
        """
          select count(id) from points where (id = {a} or id = {b}) and accountId = {accId} 
        """
      ).on( 'a -> pIdA,
            'b -> pIdB,
            'accId -> accId
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
          select typeId from points where id = {id} and accountId = {accId}
        """
      ).on( 'id -> pId, 'accId -> accId ).as(scalar[Long].singleOpt)
    }
  } 

  def add(point: Point): Option[Long] = {
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

  // update only data(json)
  def update(point: Point): Int = {
    DB.withConnection { implicit conn =>
      SQL(
        """
          update points set data = {data} where id = {pointId}
        """
      ).on(
        'data     -> point.data.toString,
        'pointId  -> point.id.get
      ).executeUpdate()
    }
  }
}
