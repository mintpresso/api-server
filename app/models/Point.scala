package models

import anorm._ 
import play.api.Play.current
import play.api.db.DB
import java.util.Date
import play.api.libs.json._
import play.api.libs.json.Json._

case class Point(id: Pk[Any], accountId: Long, typeId: Long, identifier: String, createdAt: Date, updatedAt: Date, referencedAt: Date, data: JsObject)

object Point {
  val Type: Map[String, Long] = Map(
      "user" -> 10,
      "page" -> 20,
      "post" -> 30
    )
  def apply(accId: Long, typeString: String, identifier: String, data: JsObject): Point = {
    var typeId: Long = -1
    Point.Type.get(typeString).get match {
      case id: Long => typeId = id
      case _ => throw new Exception("point(identifier=" + identifier + ") type: '" + typeString + "' isn't supported.")
    }

    if(identifier.length > 0){
      this.findOneByTypeIdAndIdentifier(accId, typeId, identifier).get match {
        case row: Point => row
        case _ => {
          new Point(anorm.NotAssigned, accId, typeId, identifier, new Date, new Date, new Date, data )
        }
      }
    }else{
      new Point(anorm.NotAssigned, accId, typeId, identifier, new Date, new Date, new Date, data )
    }
  }

  def findOneByTypeIdAndIdentifier(accId: Long, typeId: Long, identifier: String): Option[Point] = {
    DB.withConnection { implicit conn =>
      // .on('t -> typeId, 'i -> identifier)
      SQL("select * from points where accountId = {accId} and type = {typeId} and identifier = {identifier}")().map {
        //case Row(id: Long) => println(">>>>>>>>>>>>>>>" + id)
        case Row(id: Long, accId: Long, typeId: Long, identifier: String, createdAt: Date, updatedAt: Date, referencedAt: Date, data: String) => {
          Some(new Point(anorm.NotAssigned, accId, typeId, identifier, createdAt, updatedAt, referencedAt, Json.obj("data" -> Json.parse(data)) ))
        }
        //case Row(id: Long, typeId: Long, identifier: String, createdAt: Date, updatedAt: Date, referencedAt: Date, data: String) => Some(Point(anorm.id(id), type, identifier, createdAt, updatedAt, referencedAt, json.parse(data)))
      }
      None
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
