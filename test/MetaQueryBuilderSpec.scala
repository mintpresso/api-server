package test

import scala.collection.mutable.LinkedHashMap

import org.specs2.mutable._
import org.specs2.matcher.MatchResult
import play.api.Configuration
import play.api.test._
import play.api.test.Helpers._

import models.MetaQueryBuilder

class MetaQueryBuilderSpec extends Specification {
  def testContext(f: Any => MatchResult[Any]): MatchResult[Any] = {
    running(FakeApplication(additionalConfiguration = Map(
      "db.default.driver" -> "",
      "db.default.url" -> "",
      "db.default.user" -> "",
      "db.default.pass" -> ""
    ))) { 
      f()
    }
  }
  val query = "SELECT * FROM edges"
  val expectedQuery = "SELECT * FROM edges WHERE a = ? AND b = ? AND c = ?"
  val testWhere = Map("a"-> "1", "b" -> "c", "c" -> "d")

  "MetaQueryBuilder " should {
    "build query based on Map[Symbol, String] data" in {
      val query = "SELECT * FROM edges"
      val q = MetaQueryBuilder(query, where=Map('a -> "1"))
      q.sql.query === "SELECT * FROM edges WHERE a = ?"
    }

    "build query based on Map[Symbol, String] data (more longer map)" in {
      val q = MetaQueryBuilder(query, where=Map('a-> "1", 'b -> "c", 'c -> "d"))
      q.sql.query === expectedQuery
    }

    "build query based on Map[String, String]" in {
      val q = MetaQueryBuilder(query,
                               where=testWhere)
      q.sql.query === expectedQuery 

    }

    "build query based on (String, Long)*" in {
      def a(args: (String, Long)*) = {
        var tmp: Map[String, String] = Map[String, String]()
        for(a <- args) {
          tmp = tmp + (a._1 -> a._2.toString())
        }
        MetaQueryBuilder(query, where=tmp)
      }
      a("a" -> 1, "b" -> 2, "c" -> 3).sql.query === expectedQuery
    }

    "Add additional options" in {
      val q = MetaQueryBuilder(query,
                               where=testWhere,
                               additional=LinkedHashMap("LIMIT" -> "1"))
      q.sql.query === expectedQuery  + " LIMIT 1"

      
    }
  }
}
