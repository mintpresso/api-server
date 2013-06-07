package models

import play.api.Play.current
import play.api.db._
import anorm._ 
import anorm.SqlParser._
import java.util.Date

case class PointType(id: Pk[Long], name: String)

object PointType {

  val parser = {
    get[Pk[Long]]("id")~
    get[String]("name") map {
      case i~n => PointType(i, n)
    }
  }

  def findOneByName(name: String): Option[PointType] = {
    DB.withConnection { implicit con =>
      SQL(
        """
          SELECT *
          FROM pointTypes
          WHERE name = {name}
        """
      ).on(
        'name -> name
      ).as(parser.singleOpt)
    }
  }

  def findOneByNameOrAdd(name: String): PointType = {
    DB.withConnection { implicit conn =>
      SQL(
        """
          INSERT IGNORE INTO pointTypes
          (name)
          VALUES
          ({name})
        """
      ).on(
        'name -> name
      ).executeInsert()
      SQL(
        """
          SELECT *
          FROM pointTypes
          WHERE name = {name}
        """
      ).on(
        'name -> name
      ).as(parser.single)
    }
  }

  def findAllByAccountId(accId: Long): List[String] = {
    DB.withConnection { implicit con =>
      SQL(
        """
          SELECT DISTINCT pt.name
          FROM pointTypes pt, points p
          WHERE pt.id = p.typeId
            AND p.accountId = {id}
        """
      ).on(
        'id -> accId
      ).as(
        str("name") *
      )
    }
  }

  def findOneById(ptId: Long): Option[PointType] = {
    DB.withConnection { implicit con =>
      SQL(
        """
          SELECT *
          FROM pointTypes
          WHERE id = {id}
        """
      ).on(
        'id -> ptId
      ).as(
        parser singleOpt
      )
    }
  }

  def add(pt: PointType): Option[Long] = {
    DB.withConnection { implicit con =>
      SQL(
        """
          INSERT INTO pointTypes
          (name)
          VALUES
          ({name})
        """
      ).on(
        'name -> pt.name
      ).executeInsert()
    }
  }
}