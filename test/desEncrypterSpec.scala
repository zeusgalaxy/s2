package test

import org.specs2.mutable._

object desEncrypterSpec extends Specification {

  import utils._

  val testString = "This is a test string for encryption"
  var encrypted = ""
  var dE: DesEncrypter = null

  "the desEncrypter " should {

    "Create a new desEncrypter object with passphrase" in  {
      dE = new DesEncrypter(DesEncrypter.PASSPHRASE_API_KEY)
      dE mustNotEqual (null)
    }
    "Encrypt a string" in {
       encrypted = dE.encrypt(testString)
       encrypted must not beEmpty
    }
    "Decrypt the string and have it match the original " in {
      dE.decrypt(encrypted) must equalTo (testString)
    }

  }
}

