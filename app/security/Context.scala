package security

import models._
import play.api.mvc.{WrappedRequest, Request}

case class Context(var user: Option[Person], rights: Rights)

object Context {

  def apply(p: Person, t: Target): Context = {
    new Context(Some(p), Rights(Some(p), t))
  }
  def apply(t: Target): Context = {
    new Context(None, Rights(None, t))
  }
}
