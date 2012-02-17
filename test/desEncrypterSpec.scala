package test

import org.specs2.mutable._

import play.api.test._
import play.api.test.Helpers._
import play.api.mvc.AnyContentAsXml
import play.api.Logger


object desEncrypterSpec extends Specification {

  import utils._
  
  "the desEncrypter " should {

    val testString = "This is a test string for encryption"
    var encrypted = ""
    val enc = new DesEncrypter(DesEncrypter.PASSPHRASE_API_KEY)

    "Encrypt a string" in {
       encrypted = enc.encrypt(testString)
       encrypted must not beEmpty
    }

    "Decrypt the string and have it match the original " in {
      enc.decrypt(encrypted) must equalTo (testString)
    }

  }
}

