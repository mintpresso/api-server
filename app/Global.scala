import play.api._
import play.api.mvc._
import play.api.mvc.Results._
import play.api.libs.concurrent._
import play.api.Play.current

import anorm._

object Global extends GlobalSettings {

  override def onStart(app: Application) {
    // Logger.info("Application has started")
  }  
  
  override def onStop(app: Application) {
    // Logger.info("Application shutdown...")
  }

  override def onError(request: RequestHeader, ex: Throwable) = {
    InternalServerError(
      views.html.error(500, request, None)
    )
  }

  override def onHandlerNotFound(request: RequestHeader): Result = {
    NotFound(
      views.html.error(404, request, None)
    )
  }

  override def onBadRequest(request: RequestHeader, error: String) = {
    Logger.info("BadRequest: " + error)
    BadRequest(
    	views.html.error(400, request, Some(error))
    )
  }
    
}