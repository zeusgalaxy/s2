package controllers

import play.api.mvc._
import views._

object WebApp extends Controller with Secured {

  def index = Action {
    implicit request =>
      Ok(html.index("This is the main page parameter"))
  }

  def testLogin = IsAuthenticated("/testLogin", username => implicit request =>
    Ok(html.testLogin())
  )

}
