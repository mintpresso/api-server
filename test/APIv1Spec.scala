package test

import org.specs2.mutable._
import org.specs2.matcher.MatchResult

import play.api.test._
import play.api.test.Helpers._
import play.api._
import play.api.mvc._
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
import scala.concurrent.duration._

import java.util.Date

class APIv1Spec extends Specification {
  
  val accountId = 1

  val baseUrl = "http://localhost:15100/v1/"
  val baseLocalUrl = "http://127.0.0.1:15100/v1/"

  val url = baseUrl + "account/%s/".format(accountId)
  val localUrl = baseLocalUrl + "account/%s/".format(accountId)
  
  val postUrl = baseUrl + "post/account/%s/".format(accountId)
  val postLocalUrl = baseLocalUrl + "post/account/%s/".format(accountId)

  val token = "1ab4d5f5c-2316-481f-9f8f-a038e9b4bcde"
  val duration = Duration(1000, MILLISECONDS)

  "API Server" should {
    
    "return json response with status" in {
      val r = Await.result( WS.url( baseUrl + java.util.UUID.randomUUID().toString ).get(), duration)
      
      // NotFound
      r.status must equalTo(404)
      r.header("Content-Type") must beSome("application/json; charset=utf-8")
      (r.json \ "status" \ "code").as[Int] must equalTo(r.status)
    }

    "deny request without api token" in {
      val r = Await.result( WS.url( url + "point/1" ).get(), duration)
      
      // Forbidden
      r.status must equalTo(403)
    }

    "deny request with invalid api token" in {
      val r = Await.result(
        WS.url( url + "point/1" )
          .withQueryString(("api_token", "INVALID" + token))
          .get(), duration)

      // Forbidden
      r.status must equalTo(403)
    }

    "allow request with valid api token" in {
      val r = Await.result(
        WS.url( url + "point/1" )
          .withQueryString(("api_token", token))
          .get(), duration)

      // Ok
      r.status must equalTo(200)
    }

    "allow request from 127.0.0.1 with invalid api token" in {
      val r = Await.result(
        WS.url( localUrl + "point/1" )
          .withQueryString(("api_token", "INVALID" + token))
          .get(), duration)

      // Ok
      r.status must equalTo(200)
    }

    "allow request from 127.0.0.1 with valid api token" in {
      val r = Await.result(
        WS.url( localUrl + "point/1" )
          .withQueryString(("api_token", token))
          .get(), duration)

      // Ok
      r.status must equalTo(200)
    }

    "allow request from 127.0.0.1 without api token" in {
      val r = Await.result(
        WS.url( localUrl + "point/1" )
          .get(), duration)

      // Ok
      r.status must equalTo(200)
    }

    // "return 404 for unknown point" in {
    //   val r = Await.result(
    //     WS.url( localUrl + "point/0" )
    //       .get(), duration)

    //   // Ok
    //   r.status must equalTo(404)
    // }

    // "return 404 for unknown point with type" in {
    //   val r = Await.result(
    //     WS.url( localUrl + "point" )
    //       .withQueryString(("api_token", token))
    //       .get(), duration)

    //   // Ok
    //   r.status must equalTo(200)
    // }

    // "return 404 for unknown edge" in {
    //   val r = Await.result(
    //     WS.url( localUrl + "point/0" )
    //       .get(), duration)

    //   // Ok
    //   r.status must equalTo(404)
    // }

  }

  "Point" can {
    val _identifier = java.util.UUID.randomUUID().toString
    val _type = java.util.UUID.randomUUID().toString.replace("-", "")
    
    "not be added without Content-Type" in {
      val body = """
{
  "point": {
    "type": "%s"
  }
}
      """.format( _type )
      val r = Await.result(
        WS.url( localUrl + "point" )
          .post[String](body), duration)

      // Created
      r.status must equalTo(400)
    }

    "not be added with invalid typeString (number, word)" in {
      val body = """
{
  "point": {
    "type": "$-!@#~"
  }
}
      """.format( _type )
      val r = Await.result(
        WS.url( localUrl + "point" )
          .post[String](body), duration)

      // Created
      r.status must equalTo(400)
    }

    "be added by POST" in {
      val body = """
{
  "point": {
    "type": "%s"
  }
}
      """.format( _type )
      val r = Await.result(
        WS.url( localUrl + "point" )
          .withHeaders( ("Content-Type", "application/json") )
          .post[String](body), duration)

      // Created
      r.status must equalTo(201)
      (r.json \ "point" \ "type").as[String] must equalTo( _type )
    }

    "be added by GET with json parameter" in {
      val body =
      """
      {
        "point": {
          "type": "%s"
        }
      }
      """.format( _type )
      val r = Await.result(
        WS.url( postLocalUrl + "point" )
          .withQueryString(("json", body.toString))
          .get(), duration)

      // Created
      r.status must equalTo(201)
      (r.json \ "point" \ "type").as[String] must equalTo( _type )
    }

    var createdAt = 0L
    var updatedAt = 0L
    var referencedAt = 0L

    "be added with {type, identifier}" in {
      val body =
      """
      {
        "point": {
          "type": "%s",
          "identifier": "%s"
        }
      }
      """.format( _type, _identifier )
      val r = Await.result(
        WS.url( localUrl + "point" )
          .withHeaders( ("Content-Type", "application/json") )
          .post[String](body), duration)
      
      r.status must equalTo(201)
      (r.json \ "point" \ "type").as[String] must equalTo( _type )
      (r.json \ "point" \ "identifier").as[String] must equalTo( _identifier )
      createdAt = (r.json \ "point" \ "createdAt").as[Long]
      updatedAt = (r.json \ "point" \ "updatedAt").as[Long]
      referencedAt = (r.json \ "point" \ "referencedAt").as[Long]
      updatedAt must equalTo(createdAt)
      referencedAt must equalTo(createdAt)
    }

    "not be newly added with duplicate identifier" in {
      val body =
      """
      {
        "point": {
          "type": "%s",
          "identifier": "%s"
        }
      }
      """.format( _type, _identifier )
      val r = Await.result(
        WS.url( localUrl + "point" )
          .withHeaders( ("Content-Type", "application/json") )
          .post[String](body), duration)
      
      r.status must equalTo(200)
      (r.json \ "point" \ "type").as[String] must equalTo( _type )
      (r.json \ "point" \ "identifier").as[String] must equalTo( _identifier )
      (r.json \ "point" \ "createdAt").as[Long] must equalTo( createdAt )
      (r.json \ "point" \ "updatedAt").as[Long] must equalTo( updatedAt )
      (r.json \ "point" \ "referencedAt").as[Long] must equalTo( referencedAt )
    }

    "not be newly added with duplicate identifier by updateIfExists=true option (no data changes)" in {
      val body =
      """
      {
        "point": {
          "type": "%s",
          "identifier": "%s"
        }
      }
      """.format( _type, _identifier )
      val r = Await.result(
        WS.url( localUrl + "point" )
          .withHeaders( ("Content-Type", "application/json") )
          .withQueryString(("updateIfExists", "true"))
          .post[String](body), duration)

      r.status must equalTo(200)
      var c = (r.json \ "point" \ "createdAt").as[Long]
      var u = (r.json \ "point" \ "updatedAt").as[Long]
      var re = (r.json \ "point" \ "referencedAt").as[Long]

      c must equalTo( createdAt )
      u must equalTo( createdAt )
      re must equalTo( createdAt )
    }

    "be newly added with duplicate identifier by updateIfExists=true option (including data changes)" in {
      val body =
      """
      {
        "point": {
          "type": "%s",
          "identifier": "%s",
          "data": {
            "changed": true
          }
        }
      }
      """.format( _type, _identifier )
      val r = Await.result(
        WS.url( localUrl + "point" )
          .withHeaders( ("Content-Type", "application/json") )
          .withQueryString(("updateIfExists", "true"))
          .post[String](body), duration)

      r.status must equalTo(201)
      var c = (r.json \ "point" \ "createdAt").as[Long]
      var u = (r.json \ "point" \ "updatedAt").as[Long]
      var re = (r.json \ "point" \ "referencedAt").as[Long]

      c must equalTo( createdAt )
      u must greaterThan( updatedAt )
      re must greaterThanOrEqualTo( u )

      createdAt = c
      updatedAt = u
      referencedAt = re
    }

  }
  "Edge" can {
    sequential

    val _identifier = java.util.UUID.randomUUID().toString
    val _type = java.util.UUID.randomUUID().toString.replace("-", "")
    def pointSetup(f: JsValue => JsValue => MatchResult[Any]): MatchResult[Any] = {
      val body =
      """
      {
        "point": {
          "type": "%s",
          "identifier": "%s"
        }
      }
      """
      val s = Await.result(
        WS.url( localUrl + "point" )
          .withHeaders( ("Content-Type", "application/json") )
          .withQueryString(("updateIfExists", "true"))
          .post[String](body.format( _type, _identifier )),
        duration
      )
      val o = Await.result(
        WS.url( localUrl + "point" )
          .withHeaders( ("Content-Type", "application/json") )
          .withQueryString(("updateIfExists", "true"))
          .post[String](body.format( _type + "object", _identifier + "object")),
        duration
      )
      if((s.status == 201 && o.status == 201) ||  (s.status == 200 && o.status == 200)) {
        f(s.json)(o.json)
      } else {
        false === true
      }
    }

    def edgeSetup(f: Map[String, String] => MatchResult[Any]): MatchResult[Any] = {
      pointSetup { s => o =>
        val st = (s \ "point" \ "type").as[String]
        val ot = (o \ "point" \ "type").as[String]
        val si = (s \ "point" \ "identifier").as[String]
        val oi = (o \ "point" \ "identifier").as[String]
        val edgeBody = """
        {
          "edge": {
            "subjectType": "%s",
            "subjectId": "%s",
            "objectType": "%s",
            "objectId": "%s",
            "verb": "like"
          }
        }
        """.format(st, si, ot, oi)
        val r = Await.result(
          WS.url( localUrl + "edge" )
            .withHeaders( ("Content-Type", "application/json") )
            .post[String](edgeBody),
          duration
        )
        if(r.status == 201 || r.status == 200) {
          f(Map("subjectType" -> st,
                "subjectIdentifier" -> si,
                "objectType" -> ot,
                "objectIdentifier" -> oi))
        } else {
          false === true
        }
      }
    }
    "not be added Content-Type" in {
      pointSetup { s => o =>
        val st = (s \ "point" \ "type").as[String]
        val ot = (o \ "point" \ "type").as[String]
        val si = (s \ "point" \ "identifier").as[String]
        val oi = (o \ "point" \ "identifier").as[String]
        val edgeBody = """
        {
          "edge": {
            "subjectType": "%s",
            "subjectId": "%s",
            "objectType": "%s",
            "objectId": "%s",
            "verb": "like"
          }
        }
        """.format(st, si, ot, oi)
        val r = Await.result(
          WS.url( localUrl + "edge" )
            .post[String](edgeBody),
          duration
        )
        r.status === 400
      }
    }
    "be added subject and object" in {
      pointSetup { s => o =>
        val st = (s \ "point" \ "type").as[String]
        val ot = (o \ "point" \ "type").as[String]
        val si = (s \ "point" \ "identifier").as[String]
        val oi = (o \ "point" \ "identifier").as[String]
        val edgeBody = """
        {
          "edge": {
            "subjectType": "%s",
            "subjectId": "%s",
            "objectType": "%s",
            "objectId": "%s",
            "verb": "like"
          }
        }
        """.format(st, si, ot, oi)
        val r = Await.result(
          WS.url( localUrl + "edge" )
            .withHeaders( ("Content-Type", "application/json") )
            .post[String](edgeBody),
          duration
        )
        r.status === 201
      }
    }
    "be founded by subject and object with milliseconds" in {
      edgeSetup { e =>
        val r = Await.result(
          WS.url( localUrl + "edge" )
            .withQueryString(
              "subjectType" -> e("subjectType"),
              "subjectIdentifier" -> e("subjectIdentifier"),
              "objectType" -> e("objectType"),
              "objectIdentifier" -> e("objectIdentifier"),
              "getInnerPoints" ->  "false")
            .get(),
          duration
        )
        val json = Json.parse(r.body)
        val edge = (json \ "edges").as[JsArray].value(0)
        (edge \ "createdAt").as[Long] % 1000 must not equalTo( 0L )
        r.status === 200
      }
    }
  }

}
