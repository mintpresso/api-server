package test

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

  "MetaQueryBuilder " should {
    "build query based on Map[Symbol, String] data" in {
      val query = "SELECT * FROM edges"
      val q = MetaQueryBuilder(query, where=Map('a-> "1"))
      q.sql.query === "SELECT * FROM edges WHERE a = ?"
    }

    "build query based on Map[Symbol, String] data (more longer map)" in {
      val query = "SELECT * FROM edges"
      val q = MetaQueryBuilder(query, where=Map('a-> "1", 'b -> "c", 'c -> "d"))
      q.sql.query === "SELECT * FROM edges WHERE a = ? AND b = ? AND c = ?"
    }

    "build query based on Map[String, String]" in {
      
      val query = "SELECT * FROM edges"
      val q = MetaQueryBuilder(query,
                               where=Map("a"-> "1", "b" -> "c", "c" -> "d"))
      q.sql.query === "SELECT * FROM edges WHERE a = ? AND b = ? AND c = ?"

    }
  }
}
