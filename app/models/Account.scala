package models

import anorm._ 
import anorm.SqlParser._
import play.api.Play.current
import play.api.db.DB
import java.util.Date
import play.api.libs.Crypto
import play.api.mvc._
import play.api.Logger

case class Account(id: Pk[Any], email: String, name: String, var password: String, api_token: String) {
}

object Account {
  val parser = {
    get[Pk[Long]]("id")~
    get[String]("email")~ 
    get[String]("password")~ 
    get[String]("name")~
    get[String]("api_token") map {
      case pk~s1~s2~s3~s4 => {
        new Account(pk, s1, s3, s2, s4)
      }
    }
  }

  def apply(email: String, name: String, password: String): Account = {
    new Account(anorm.NotAssigned, email, name, password, "")
  }
  def add(obj: Account) = {
    DB.withConnection { implicit conn =>
      val row = SQL("select count(email) as c from accounts where email = {email}").on('email -> obj.email).apply().head
      if(row[Option[Long]]("c").get > 0 ){
        throw new Exception("Duplicated email")
      }
      
      /*
      if( row[Option[Long]]("c").getOrElse(None) > 0 ){
        throw new Exception("Duplicated email")
      }
      */
      val id: Option[Long] = SQL(
        """
          insert into accounts(email, name, password, api_token)
          values ({email}, {name}, {password}, {token})
        """
      ).on(
        'email -> obj.email, 
        'name -> obj.name, 
        'password -> obj.password,
        'token -> ""
      ).executeInsert()
      id
    }
  }
  
  def findOneById(id: Long): Option[Account] = {
    DB.withConnection { implicit conn =>
      SQL(
        """
          select * from accounts where id = {id}
        """
      ).on( 'id -> id ).singleOpt(parser)
    }
  }

  def findOneByEmail(email: String): Option[Account] = {
    DB.withConnection { implicit conn =>
      SQL(
        """
          select * from accounts where email = {email}
        """
      ).on( 'email -> email ).singleOpt(parser)
    }
  }

  def count(): Option[Long] = {
    DB.withConnection { implicit conn =>
      val row = SQL("select count(id) as c from accounts").apply().head
      row[Option[Long]]("c")
    }
  }

  def updateToken(id: Long, token: String) = {
    DB.withConnection { implicit conn =>
      SQL(
        """
          update accounts set api_token = {token} where id = {id}
        """
      ).on( 'id -> id, 'token -> token ).execute()
    }
  }

  def verifyToken(id: Long, token: String, request: RequestHeader): Boolean = {
    val d = Crypto.decryptAES(token)
    val ll = id.toString.length
    val i = d.substring(0, ll)
    if(i != id.toString){
      Logger.warn("Given ID is unmatched with the id signed in token.")
      false
    }else{
      //Logger.info("COMPARE: domain(" + request.domain + "), remoteAddress(" + request.remoteAddress + "), filter(" + d.substring(ll) + ") ")
      //val u: Array[String] = d.substring(ll).split(",")
      true
    }
  }
}
