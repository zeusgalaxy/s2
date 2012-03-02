package utils

import scalaz._
import com.sun.org.apache.xml.internal.security.utils.Base64
import javax.crypto._
import javax.crypto.spec._
import java.security.spec._
import java.io._
import java.security.{NoSuchAlgorithmException, InvalidAlgorithmParameterException}
import javax.management.openmbean.InvalidKeyException
import com.sun.org.apache.xml.internal.security.exceptions.Base64DecodingException
import play.api.Logger

// A version of the old way of encrypting the admin passwords. Doesn't match so let's move on.
//
//import org.mortbay.jetty.security.Credential;
//import java.security.MessageDigest
//def md5(s: String) = {
// MessageDigest.getInstance("MD5").digest(s.getBytes)
//}

/**
 *  Encrypt the specified string.
 *  This interoperates with the web password on Dino. It does not work with the MD5 Digest passwords
 *  Be very careful about the charset. If they are different on Dino or here then the interop won't
 *  work!
 *
 *  Matches up with code in Dino:  src/java/com/netpulse/util/StringUtil
 */
object Blowfish {

  implicit val loc = VL("desEncrypter.Blowfish")

  def encrypt (encryptMe: String): String = {
    validate {
      val key = new SecretKeySpec ("jaas is the way".getBytes("UTF-8"), "Blowfish")
      val cipher: Cipher = Cipher.getInstance ("Blowfish")

      cipher.init(Cipher.ENCRYPT_MODE, key)
      val encoding = cipher.doFinal(encryptMe.getBytes("UTF-8"))
      val n = new BigInt(new java.math.BigInteger(encoding))
      n.toString(16)
    }.error(Map("msg" -> "Encryption Error")).fold(e => "", s => s)
  }
}

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

  implicit val loc = VL("desEncrypter")

  // Constructor
  validate {
    var keySpec: KeySpec = new PBEKeySpec(passPhrase.toCharArray, salt, iterationCount)
    var key: SecretKey = SecretKeyFactory.getInstance("PBEWithMD5AndDES").generateSecret(keySpec)
    ecipher = Cipher.getInstance(key.getAlgorithm)
    dcipher = Cipher.getInstance(key.getAlgorithm)
    var paramSpec: AlgorithmParameterSpec = new PBEParameterSpec(salt, iterationCount)
    ecipher.init(Cipher.ENCRYPT_MODE, key, paramSpec)
    dcipher.init(Cipher.DECRYPT_MODE, key, paramSpec)
  }.error(Map("msg" -> "Constructor Error"))


  def encrypt(str: String): String = validate {

      var utf8: Array[Byte] = str.getBytes("UTF8")
      var enc: Array[Byte] = ecipher.doFinal(utf8)

      Base64.encode(enc)
    }.error(Map("msg" -> "encrypt Error")).fold(e => "", s => s)


  def decrypt(str: String): String = validate {

      var dec: Array[Byte] = Base64.decode(str)
      var utf8: Array[Byte] = dcipher.doFinal(dec)

      new String(utf8, "UTF8")
    }.error(Map("msg" -> "decrypt Error")).fold(e => "", s => s)

}