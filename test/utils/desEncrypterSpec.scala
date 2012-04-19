package test.utils

import org.specs2.mutable._

object desEncrypterSpec extends Specification {

  import utils._

  val testString = "This is a test string for encryption"
  var encrypted = ""
  var dE: DesEncrypter = null


  "the BlowfishEncrypter" should {
    "Encrypt this string correctly" in {
      Blowfish.encrypt(testString) must equalTo("-7025642f40677a6b7de24c9b0f57d063bf9d0bed6aa455ef98e1d75239624ebe848d6241be2fff2a")
    }
  }

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

