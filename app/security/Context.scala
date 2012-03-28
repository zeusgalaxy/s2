package security

import models._
import play.api.mvc.{WrappedRequest, Request}

case class Context(var user: Option[User], rights: Rights)

object Context {

  def apply(u: User, t: Target): Context = {
    new Context(Some(u), Rights(Some(u), t))
  }
  def apply(t: Target): Context = {
    new Context(None, Rights(None, t))
  }
}
