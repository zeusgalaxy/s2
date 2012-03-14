package controllers

import models._
import play.api.mvc.{WrappedRequest, Request}

case class Context(user: User, rights: Rights)

object Context {
  def apply(u: Option[User], t: Target, defaultRights: Rights = noRights) = {
    Context(u, Rights(u, t, defaultRights))
  }
}
case class ContextualizedRequest[A](user: User, override val request: Request[A]) extends WrappedRequest(request)
