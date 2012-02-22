package utils

import com.sun.org.apache.xml.internal.security.utils.Base64
import javax.crypto._
import javax.crypto.spec._
import java.security.spec._
import java.io._
import java.security.{NoSuchAlgorithmException, InvalidAlgorithmParameterException}
import javax.management.openmbean.InvalidKeyException
import com.sun.org.apache.xml.internal.security.exceptions.Base64DecodingException
import play.api.Logger

/**
 *
 * @author
 * Adapted from http://exampledepot.com/egs/javax.crypto/PassKey.html
 *
 */
object DesEncrypter {
  final val PASSPHRASE_API_KEY: String = "Netpulse API Key passphrase"
  final val SESSION_SECRET_KEY: String = "The muni trains noisily pass by."
}

class DesEncrypter ( passPhrase: String ) {
  private var ecipher: Cipher = null
  private var dcipher: Cipher = null
  private var salt: Array[Byte] = Array(0xA9.asInstanceOf[Byte], 0x9B.asInstanceOf[Byte], 0xC8.asInstanceOf[Byte], 0x32.asInstanceOf[Byte], 0x56.asInstanceOf[Byte], 0x35.asInstanceOf[Byte], 0xE3.asInstanceOf[Byte], 0x03.asInstanceOf[Byte])
  private var iterationCount: Int = 19

  // Constructor
  try {
    var keySpec: KeySpec = new PBEKeySpec(passPhrase.toCharArray, salt, iterationCount)
    var key: SecretKey = SecretKeyFactory.getInstance("PBEWithMD5AndDES").generateSecret(keySpec)
    ecipher = Cipher.getInstance(key.getAlgorithm)
    dcipher = Cipher.getInstance(key.getAlgorithm)
    var paramSpec: AlgorithmParameterSpec = new PBEParameterSpec(salt, iterationCount)
    ecipher.init(Cipher.ENCRYPT_MODE, key, paramSpec)
    dcipher.init(Cipher.DECRYPT_MODE, key, paramSpec)
  }
  catch {
    case e: InvalidAlgorithmParameterException => {
    }
    case e: InvalidKeySpecException => {
    }
    case e: NoSuchPaddingException => {
    }
    case e: NoSuchAlgorithmException => {
    }
    case e: InvalidKeyException => {
    }
  }

  def encrypt(str: String): String = {
    try {
      var utf8: Array[Byte] = str.getBytes("UTF8")
      var enc: Array[Byte] = ecipher.doFinal(utf8)
      return Base64.encode(enc)
    }
    catch {
      case e: BadPaddingException => {
      }
      case e: IllegalBlockSizeException => {
      }
      case e: UnsupportedEncodingException => {
      }
      case e: IOException => {
      }
    }
    return null
  }

  def decrypt(str: String): String = {
    try {
      Logger.debug("are equal: " + (str == "fIOiwo2X1Qt4+LF+BIuHNRyKMJDzgicjcTzMt5+rWKcU9hOCgMa34vDGj0teYEL0TnsIlUMLR8BY\r\nJK4de4XTlrKTCjC1+zsQUXrHRTADC7M=\r\n").toString)
      var dec: Array[Byte] = Base64.decode(str)
      var utf8: Array[Byte] = dcipher.doFinal(dec)

      return new String(utf8, "UTF8")
    }
    catch {
      case e: Base64DecodingException => {
        Logger.debug("Base64DecodingException: "+str+"\n"+e.getMessage)
      }
      case e: BadPaddingException => {
      }
      case e: IllegalBlockSizeException => {
      }
      case e: UnsupportedEncodingException => {
      }
      case e: IOException => {
      }
    }
    return null
  }

}