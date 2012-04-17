package test.sample

import utils._
import org.specs2.mutable._

import models._
import sample._
import play.api.test._
import play.api.test.Helpers._
import play.api.Play._
import play.api.mvc.Session

object MySampleSpec extends Specification {

  "Get both real and mock values from MyExternalWs" in {

    running(FakeApplication()) {
      import sample.MyFactory._
      val wsReal: MyExternalWsLike = implicitly[MyExternalWsLike]
      wsReal.getX must equalTo(1)
      wsReal.getY must equalTo("real")
    }
    running(FakeApplication()) {

      SampleGlobal.useMocks = true
      Env.mockDb = true
      Env.mockVT = true
      Env.mockSomeElse = true
      Env.mockAllBut(x,y)
      Env.mock = myPredefMock(A)

      import sample.MyFactory._
      val wsMock: MyExternalWsLike = implicitly[MyExternalWsLike]
      wsMock.getX must equalTo(2)
      wsMock.getY must equalTo("mock")
    }
  }
}