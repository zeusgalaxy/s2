/**
 * Package object with misc. utility functions, et. al.
 */

import models._

import java.io.StringWriter
import org.apache.commons.lang.RandomStringUtils
import org.joda.time.{DateTimeZone, DateTime}
import scala.xml._
import play.api._
import play.api.mvc._
import play.api.libs.ws.WS._
import play.api.libs.ws.Response
import play.api.libs.concurrent.Promise

import scalaz.{Node => _, Logger => _, _}
import Scalaz._

/**
 * General-purpose utility functions and implicits for S2.
 */
package object utils {

  val apiNoError = 0
  val apiGeneralError = 1
  val apiVtRegistrationUserExists = 2
  val apiVtRegistrationUnableToGetStatus = 3
  val apiVtRegistrationOtherError = 4
  val apiUnableToRetrieveExerciser = 5

  val vtStatusLoggedIn = 3
  val vtStatusLinked = 2
  val vtStatusUnlinked = 1
  val vtStatusUnknown = 0

  /**Replaces the subdomain portion of a request received by S2 so it may be forwarded
   * on for further processing by Dino. The replacement logic is:
   *
   * localhost:9000 --> localhost:8080
   * ec2 --> s1
   * s2 --> s1
   *
   * The idea is that we came into S2 either because 1) we are running play locally, in which case we are
   * presumably also running Dino locally on port 8080, or 2) because the client specified "ec2" as the
   * subdomain and the front-end routed to here, or 3) because the client specified "s2" explicitly.
   * In either of these last two cases, we need to be explicit with "s1" as the subdomain so that
   * it can route properly to Dino.
   *
   * @param s The string containing the host value as it arrived here in S2.
   * @return The host string with the above substitutions applied.
   */
  def switchHosts(s: String) = s.replaceFirst("localhost:9000", "localhost:8080").
    replaceFirst("ec2", "s1").replaceFirst("s2", "s1")

  /**Repackages an incoming request into an outgoing request so it can be sent to Dino
   * for processing. By the time we are processing the request here, it has already
   * been decomposed out of its raw state, and we thus must rebuild that raw state before it can
   * be executed again. As part of the repackaging, the target host will be modified so that
   * it points to Dino instead of S2.
   *
   * @param r The request that needs to be repackaged.
   * @return A tuple with a WSRequestHolder representing the request, and an Option with
   * the request's body, if applicable.
   */
  def toWSRequest(r: Request[AnyContent]): (WSRequestHolder, Option[Array[Byte]]) = {

    val newBody: Option[Array[Byte]] = r.body match {
      case c@AnyContentAsFormUrlEncoded(data) => None
      case c@AnyContentAsText(txt) => Some(txt.getBytes)
      case c@AnyContentAsXml(xml) => Some(xml.text.getBytes)
      case c@AnyContentAsJson(json) => Some(json.toString().getBytes)
      case c@AnyContentAsMultipartFormData(mfd) => None
      case c@AnyContentAsRaw(raw) => raw.asBytes()
      case _ => None
    }
    val pth = r.host.isEmpty ? switchHosts(r.uri) | "http://" + switchHosts(r.host) + r.uri // Weirdness with specs2 FakeRequest
    (WSRequestHolder(pth, Map("ACCEPT-CHARSET" -> Seq("utf-8")), Map(), None, None, None, None), newBody)
  }

  /**Executes a Promise of a Response with a specified wait value, since the default timeout of
   * five seconds may not be adequate in all situations.
   *
   * @param p The Promise of the Reponse.
   * @param timeout The timeout to use for the execution of this Promise.
   * @return The Reponse, once available.
   */
  def waitVal(p: Promise[Response], timeout: Int): Response = {
    p.await(timeout)
    p.value.get
  }

  /**Expresses the current time in the UTC timezone in milliseconds.
   *
   * @return Now in the UTC timezone in milliseconds.
   */
  def utcNowInMillis = DateTime.now(DateTimeZone.UTC).getMillis

  /**Expresses the current time in the local timezone in milliseconds.
   *
   * @return Now in the local timezone in milliseconds.
   */
  def utcNowInSecs = utcNowInMillis / 1000

  /**Calculates a pseudo-nonce. The probability of a collision (i.e., the nonce has already been used)
   * is a maximum of 1 in 36^6, and that's assuming multiple requests made per millisecond. At
   * lower request rates, likelihood of uniqueness is even higher.
   *
   * @return String with a psuedo-nonce.
   */
  def nonce = utcNowInMillis.toString + RandomStringUtils.randomAlphanumeric(6)

  /**
   * A BASE64 encoder that can be used to base64 encode strings.
   */
  lazy val b64Enc = new sun.misc.BASE64Encoder()


  /**Strips an xml string of its header (such as: <?xml version="1.0" encoding="UTF-8" standalone="yes"?>)
   *
   * @param xml The xml string to be stripped.
   * @return The input xml minus any header like <?xml version="1.0" encoding="UTF-8" standalone="yes"?>
   */
  def noHdr(xml: String): String = {

    val X = """(^<\?xml.*?>)?(.*)""".r
    val X(hdr, body) = xml
    body
  }

  /**Strips an xml string of any occurrences of a specified tag.
   *
   * @param xml The xml string to be stripped.
   * @param tag The tag name to be stripped out of the xml string.
   * @return The input xml minus any of the specified elements.
   */
  def noTag(xml: String, tag: String): String = {

    val s = """(.*?)?(<""" + tag + """.*?>)(.*?)(</""" + tag + """>)?(.*)?"""
    val X = s.r
    val X(p1, t1, p2, t2, p3) = xml
    p1 + p2 + p3
  }

  /**Produces a textual representation of an xml structure, in utf-8 encoding, and including
   * the xml prologue.
   *
   * @param root The root node of an xml structure.
   * @return String of valid xml text.
   */
  def writeXml(root: Node) = {

    val writer: StringWriter = new StringWriter()
    XML.write(writer, root, "utf-8", true, null)
    writer.toString
  }

  /**Executes a given block of code and then applies a predicate expression to that block's result value.
   * If the predicate fails, the result will be a ValidationNEL, with the error message either a default
   * that indicates the condition failed, or a message passed in as part of the construct. The predicate
   * is only applied if the block was a success, of course.
   *
   * @param body The code to be executed.
   * @param postCond The condition to be applied to the result of the body's execution.
   * @param msg The text to use as the validation failure, if the predicate yields false.
   * @return A ValidationNEL that will be success if both the block and the predicate were successful.
   */
  def tst[T](body: => T)(postCond: (T => Boolean) = {
    _: T => true
  }, msg: String = "Failed post condition"): ValidationNEL[String, T] = {

    try {
      val res = body
      if (postCond(res)) res.success else msg.failNel
    } catch {
      case e => e.getMessage.failNel
    }
  }

  /**Executes a give block of code and returns the result as a ValidationNEL success if no problems, otherwise
   * as a failure. The value of using this "wrapper" function is that it automatically catches any exceptions
   * that may be thrown, and transforms them into Validation failures.
   *
   * @param body The code to be executed.
   * @return A ValidationNEL with success if the block of code executed successfully; otherwise a failure.
   */
  def vld[T](body: => T): ValidationNEL[String, T] = {

    tst(body)({
      _: T => true
    })
  }

  /**
   * Extends [[scala.Option]] with additional functionality via the pimp-by-library technique. Implicit
   * conversions from [[scala.Option]] are provided.
   */
  class NPOption[T](val o: Option[T]) {

    /**Promotes the result of a get operation to a ValidationNEL, so that even unsafe Option "gets" can be
     * performed. If the Option is a None, the result will be a ValidationNEL failure instead of a success.
     * This is convenient for accessing Options within a for comprehension who's container is a ValidationNEL.
     *
     * @param msg The failure message to be used if the retrieval fails.
     * @return A ValidationNEL success with the Option's value, if sucessful; else a failure with the provided message.
     */
    def getOrFail(msg: String): ValidationNEL[String, T] = {
      o match {
        case Some(v) => v.success
        case _ => msg.failNel
      }
    }
  }

  /**
   * Implicit conversion that allows [[utils.NPOption]] functionality to be available on any [[scala.Option]].
   */
  implicit def optToNPOption[T](o: Option[T]): NPOption[T] = {
    new NPOption(o)
  }


  /**String wrapper to allow code location to be passed implicitly to [[utils.NPValidationNEL]].
   *
   * @param loc Description of code location, to appear along with error text when a Validation is logged.
   */
  case class VL(loc: String)

  implicit def VLtoString(vl: VL) = vl.loc

  type ValMsgs = Map[String, String]

  /**
   * Extends [[scalaz.Validation]] with additional functionality via the pimp-by-library technique.
   * In particular, provides the ability to 1) automatically capture thrown exceptions and convert
   * them into validation failures (via the "vld" and "tst" functions); 2) conditionally log any
   * failures, using different logging levels; 3) conditionally append additional text to be
   * logged in case of a failure (via "add"); ability retrieve a success value or throw an error
   * in case of failure, which will be caught by an encompassing vld or tst block. (This is useful
   * for short-circuiting a series of calculations.) The failure type of this structure is always
   * a [[scalaz.NonEmptyList]] of Strings.
   *
   * Implicit conversions from [[scalaz.Validation]] are provided.
   */
  class NPValidationNEL[T](val v: Validation[NonEmptyList[String], T]) {

    /**Yields a properly formatted string for purpose of writing a log entry; includes
     * all failure strings that have been accumulated. The log entry format is (tab separated)
     * log level, source code location, failure messages (comma separated).
     *
     * @param src The source code location associated with this validation block.
     * @return A property formatted failure string, suitable for logging.
     */
    def logTxt(src: String) =
      "\t" + src + "\t" + v.fold(e => e.list.mkString(", "), s => "") + "\t"

    /**Retrieves the success value, or throws an error if a failure. This is intended to be used within
     * a "vld" or "tst" construct, so the error can be caught converted into a ValidationNEL. This is a form
     * of shortcircuiting, and is not recommended. Using for comprehensions or mapping/folding techniques is preferred.
     *
     * @return Success value of validation, if available; else an error is thrown.
     */
    def getOrThrow = v.fold(e => throw new Exception("Error(s): " + e.list.mkString(", ")), s => s)

    /**Appends additional text into the NonEmptyList of failure messages, if the validation is a
     * failure. If it's a success, the operation has no effect.
     *
     * @param k A descriptive "key" that can be inserted along with the actual message text.
     * @param msg The text that is to be added to the list of failure strings.
     * @return This validation, so operations can be strung together.
     */
    def add(k: String, msg: String): Validation[NonEmptyList[String], T] = {
      // Ignore the syntax highlight, below; IntelliJ is confused.
      if (v.isFailure) (v <* ("Additional info: " + k + ": " + msg).failNel[T]) else v
    }

    /**Log any failure messages to the logger, using the trace logging level, if this validation
     * is a failure. If a success, do nothing.
     *
     * @param src A textual description of where the operation is coming from in the source code.
     * @return This validation, so operations can be chained together.
     */
    def trace(implicit src: VL): Validation[NonEmptyList[String], T] = {
      if (v.isFailure) Logger.trace("TRACE" + logTxt(src))
      v
    }


    /**Log any failure messages to the logger, using the debug logging level, if this validation
     * is a failure. If a success, do nothing.
     *
     * @param src A textual description of where the operation is coming from in the source code.
     * @return This validation, so operations can be chained together.
     */
    def debug(implicit src: VL): Validation[NonEmptyList[String], T] = {
      if (v.isFailure) Logger.debug("DEBUG" + logTxt(src))
      v
    }


    /**Log any failure messages to the logger, using the ifno logging level, if this validation
     * is a failure. If a success, do nothing.
     *
     * @param src A textual description of where the operation is coming from in the source code.
     * @return This validation, so operations can be chained together.
     */
    def info(implicit src: VL): Validation[NonEmptyList[String], T] = {
      if (v.isFailure) Logger.info("INFO" + logTxt(src))
      v
    }


    /**Log any failure messages to the logger, using the warn logging level, if this validation
     * is a failure. If a success, do nothing.
     *
     * @param src A textual description of where the operation is coming from in the source code.
     * @return This validation, so operations can be chained together.
     */
    def warn(implicit src: VL): Validation[NonEmptyList[String], T] = {
      if (v.isFailure) Logger.warn("WARN" + logTxt(src))
      v
    }


    /**Log any failure messages to the logger, using the error logging level, if this validation
     * is a failure. If a success, do nothing.
     *
     * @param src A textual description of where the operation is coming from in the source code.
     * @return This validation, so operations can be chained together.
     */
    def error(implicit src: VL): Validation[NonEmptyList[String], T] = {
      if (v.isFailure) Logger.error("ERROR" + logTxt(src))
      v
    }
  }

  /**Implicit conversion from [[scalaz.Validation]] to [[utils.NPValidationNEL]].
   *
   */
  implicit def vToNPValidationNEL[T](v: Validation[String, T]): NPValidationNEL[T] = {
    new NPValidationNEL(v.liftFailNel)
  }

  /**Implicit conversion from [[utils.NPValidationNEL]] to [[scalaz.Validation]]
   *
   */
  implicit def nelToValidationNEL[T](np: ValidationNEL[String, T]): NPValidationNEL[T] = {
    new NPValidationNEL(np)
  }

  /**dateToInt - convert a date string to an int
   *
   * @param dateStr  String formatted thusly - yyyy-mm-dd
   * @param default  If there is an error what value do you want returned?
   * @return an integer date e.g. 20120319 or the default
   */
  def dateToInt(dateStr: String,  default: Int = 0) = {
    try{ (dateStr filter (_ != '-'  )).toInt } catch { case _ => default }
  }
}