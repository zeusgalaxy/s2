
package sample

import scalaz._
import Scalaz._

object SampleGlobal {

  var useMocks = false

}

package object MyFactory {

//  play.api.Play.current.global.
  private lazy val mockExtWs = new MyExternalWsMock
  private lazy val realExtWs = new MyExternalWsReal
  implicit def myExternalWs: MyExternalWsLike = if (SampleGlobal.useMocks) mockExtWs else realExtWs
}