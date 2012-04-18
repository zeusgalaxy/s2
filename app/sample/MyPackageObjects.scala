
package sample

import scalaz._
import Scalaz._

package object Factory {

  implicit val defaultApp: ClassSampleApp = new ClassSampleApp
                              with TraitController
                              with TraitDbLayer
                              with TraitExternalWs
                              with TraitHandler

}
