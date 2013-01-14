package models

import anorm._ 
import play.api.Play.current
import play.api.db.DB
import java.util.Date

case class Account(id: Pk[Any], email: String, name: String, var password: String) {
}

object Account {
  def apply(email: String, name: String, password: String): Account = {
    new Account(anorm.NotAssigned, email, name, password)
  }
  def add(obj: Account) = {
    val md = java.security.MessageDigest.getInstance("SHA-1")
    obj.password = new sun.misc.BASE64Encoder().encode(md.digest(obj.password.getBytes))
    
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
          insert into accounts(email, name, password)
          values ({email}, {name}, {password})
        """
      ).on(
        'email -> obj.email, 
        'name -> obj.name, 
        'password -> obj.password
      ).executeInsert()
      id
    }
  }
  
  def findOneById(id: Long): Option[Account] = {
    DB.withConnection { implicit conn =>
      val rowStream = SQL("select * from accounts where id = {id}").on('id -> id).apply()
      if(rowStream.isEmpty){
        None
      }else{
        val row = rowStream.head
        Some(new Account(row[Pk[Long]]("id"), row[String]("email"), row[String]("name"), ""))
      }
    }
  }

  def count(): Option[Long] = {
    DB.withConnection { implicit conn =>
      val row = SQL("select count(id) as c from accounts").apply().head
      row[Option[Long]]("c")
    }
  }
}
