import play.api._
import play.api.mvc._
import play.api.mvc.Results._
import play.api.libs.concurrent._
import play.api.Play.current
import controllers.Application

import models.PointType
import anorm._

object Global extends GlobalSettings {

  override def onStart(app: Application) {
    // Logger.info("Application has started")
  }  
  
  override def onStop(app: Application) {
    // Logger.info("Application shutdown...")
  }

  override def onError(request: RequestHeader, ex: Throwable) = {
    Application.InternalServerErrorJson(500, "Temporarily service is unavailable.")
  }

  override def onHandlerNotFound(request: RequestHeader): Result = {
    Application.NotFoundJson(404, request.toString)
  }

  override def onBadRequest(request: RequestHeader, error: String) = {
    Logger.info("BadRequest: " + error)
    Logger.info(" > " + request)
    Application.BadRequestJson(400, "BadRequest: " + error)
  }
    
}