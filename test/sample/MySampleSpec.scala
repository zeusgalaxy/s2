package test.sample

import utils._
import org.specs2.mutable._

import models._
import play.api.test._
import play.api.test.Helpers._
import play.api.Play._
import play.api.mvc.Session
import sample._

object MySampleSpec extends Specification {

  "Get both real and mock values from the application" in {

    running(FakeApplication()) {

      import sample.Factory._
      val app: ClassSampleApp = implicitly[ClassSampleApp]

      app.wholeThing must contain("TRAIT controllerPiece")
      app.wholeThing must contain("TRAIT dbLayerPiece")
      app.wholeThing must contain("TRAIT externalWsPiece")
      app.wholeThing must contain("TRAIT handlerPiece")



      trait MockDbLayer extends TraitDbLayer {
        override def dbLayerPiece = "MOCK dbLayerPiece"
      }
      trait MockHandler extends TraitHandler {
        override def handlerPiece = "MOCK handlerPiece"
      }

      val mock1: ClassSampleApp = new ClassSampleApp
        with TraitController
        with MockDbLayer
        with TraitExternalWs
        with MockHandler

      mock1.wholeThing must contain("TRAIT controllerPiece")
      mock1.wholeThing must contain("MOCK dbLayerPiece")
      mock1.wholeThing must contain("TRAIT externalWsPiece")
      mock1.wholeThing must contain("MOCK handlerPiece")
    }
  }
}