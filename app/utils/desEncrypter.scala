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



/** Digest encrypt the specified string. Can not be decrypted later. Used for comparison.
 *
 *  This interoperates with the web password on Dino. It does not work with the MD5 Digest passwords
 *  Be very careful about the charset. If they are different on Dino or here then the interop won't
 *  work!
 *
 *  Matches up with code in Dino:  src/java/com/netpulse/util/StringUtil
 */
object Blowfish {

  implicit val loc = VL("desEncrypter.Blowfish")

  /** encrypt a passed in string with a blowfish cipher.
   *
   * @param encryptMe string to encrypt
   * @return encrypted string or empty string on error.
   */
  def encrypt (encryptMe: String): String = {
    vld {
      val key = new SecretKeySpec ("jaas is the way".getBytes("UTF-8"), "Blowfish")
      val cipher: Cipher = Cipher.getInstance ("Blowfish")

      cipher.init(Cipher.ENCRYPT_MODE, key)
      val encoding = cipher.doFinal(encryptMe.getBytes("UTF-8"))
      val n = new BigInt(new java.math.BigInteger(encoding))
      n.toString(16)
    }.logError.fold(e => "", s => s)
  }
}

/** Contain the secret keys for the companion object
 *
 * Adapted from http://exampledepot.com/egs/javax.crypto/PassKey.html
 *
 */
object DesEncrypter {
  final val PASSPHRASE_API_KEY: String = "Netpulse API Key passphrase"
  final val SESSION_SECRET_KEY: String = "The muni trains noisily pass by."
}

/**
 * Encrypt/Decrypt the passed in string using a 3DES cipher.
 *
 * Interoperates with the DesEncrypter on Dino. To be used when data needs to be encrypted on either
 * side and later decrypted on the other.
 */

class DesEncrypter ( passPhrase: String ) {
  private var ecipher: Cipher = null
  private var dcipher: Cipher = null
  private var salt: Array[Byte] = Array(0xA9.asInstanceOf[Byte], 0x9B.asInstanceOf[Byte], 0xC8.asInstanceOf[Byte], 0x32.asInstanceOf[Byte], 0x56.asInstanceOf[Byte], 0x35.asInstanceOf[Byte], 0xE3.asInstanceOf[Byte], 0x03.asInstanceOf[Byte])
  private var iterationCount: Int = 19

  implicit val loc = VL("desEncrypter")

  /** Constructor can encounter secret key and Cipher creation errors **/
  vld {
    var keySpec: KeySpec = new PBEKeySpec(passPhrase.toCharArray, salt, iterationCount)
    var key: SecretKey = SecretKeyFactory.getInstance("PBEWithMD5AndDES").generateSecret(keySpec)

    ecipher = Cipher.getInstance(key.getAlgorithm)
    dcipher = Cipher.getInstance(key.getAlgorithm)

    var paramSpec: AlgorithmParameterSpec = new PBEParameterSpec(salt, iterationCount)

    ecipher.init(Cipher.ENCRYPT_MODE, key, paramSpec)
    dcipher.init(Cipher.DECRYPT_MODE, key, paramSpec)
  }.logError

  /** encrypt the passed string
   *
   * @param str
   * @return encrypted string or empty string on failure
   */
  def encrypt(str: String): String = vld {

      var utf8: Array[Byte] = str.getBytes("UTF8")
      var enc: Array[Byte] = ecipher.doFinal(utf8)

      Base64.encode(enc)
    }.logError.fold(e => "", s => s)

  /** decrypt the passed string
   *
   * @param str
   * @return encrypted string or empty string on failure
   */
  def decrypt(str: String): String = vld {

      var dec: Array[Byte] = Base64.decode(str)
      var utf8: Array[Byte] = dcipher.doFinal(dec)

      new String(utf8, "UTF8")
    }.logError.fold(e => "", s => s)

}