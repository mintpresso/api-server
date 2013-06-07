import sbt._
import Keys._
import play.Project._
import com.typesafe.config._

object ApplicationBuild extends Build {

  val conf = ConfigFactory.parseFile(new File("conf/application.conf")).resolve()
  val appName         = "api-server"
  val appVersion      = "1.0-SNAPSHOT"

  val appDependencies = Seq(
    jdbc,
    anorm,
    "mysql" % "mysql-connector-java" % "5.1.25"
  )


  val main = play.Project(appName, appVersion, appDependencies).settings(
    
  ) dependsOn (
    RootProject(uri(conf.getString("affogato.repository")))
  )

}
