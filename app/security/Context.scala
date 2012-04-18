package security

import models._
import play.api.mvc.{WrappedRequest, Request}

case class Context(var user: Option[Person], rights: Rights)

object Context extends RightsDao {

  def apply(p: Person, t: Target): Context = {
    new Context(Some(p), rtGet(Some(p), t))
  }
  def apply(t: Target): Context = {
    new Context(None, rtGet(None, t))
  }
}
