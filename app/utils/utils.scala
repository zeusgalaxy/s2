/**
 * Package object with misc. utility functions, et. al.
 */

import java.io.StringWriter
import scala.xml._
import play.api._
import play.api.mvc._
import play.api.libs.ws.WS._
import play.api.libs.ws.Response
import play.api.libs.concurrent.Promise

import scalaz.{Node => _, Logger => _, _}
import Scalaz._

package object utils {

//  def switchHosts(s: String): String = s.replaceFirst("localhost:9000", "qa-ec2.netpulse.ws").
//    replaceFirst("ec2", "s1").replaceFirst("s2", "s1")

  def switchHosts(s: String): String = s.replaceFirst("localhost:9000", "localhost:8080").
    replaceFirst("ec2", "s1").replaceFirst("s2", "s1")

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

    val qs = {
      for (k <- r.queryString.keys; v <- r.queryString.get(k).get) yield (k -> v)
    }.toMap
    (WSRequestHolder("http://" + switchHosts(r.host) + r.uri, Map("ACCEPT-CHARSET" -> Seq("utf-8")), qs, None, None), newBody)
  }

  def waitVal(p: Promise[Response], timeout: Int): Response = {
    p.await(timeout)
    p.value.get
  }

  def postOrGetParams(rq: Request[AnyContent], keys: Seq[String]): Map[String, Seq[String]] = {

    val source = rq.body.asFormUrlEncoded match {
      case Some(form) => form
      case None => rq.queryString
    }
    (for (k <- keys; v <- source.get(k)) yield (k -> v)).toMap
  }

  def noHdr(xml: String): String = {

    //    <?xml version="1.0" encoding="UTF-8" standalone="yes"?>
    val X = """(^<\?xml.*?>)?(.*)""".r
    val X(hdr, body) = xml
    body
  }

  def noTag(xml: String, tag: String): String = {

    val s = """(.*?)?(<""" + tag + """.*?>)(.*?)(</""" + tag + """>)?(.*)?"""
    val X = s.r
    val X(p1, t1, p2, t2, p3) = xml
    p1 + p2 + p3
  }

  def writeXml(root: Node) = {

    val writer: StringWriter = new StringWriter()
    XML.write(writer, root, "utf-8", true, null)
    writer.toString
  }

  def test[T](body: => T)(postCond: (T => Boolean) = { _ : T => true }): ValidationNEL[String, T] = {

    try {
      val res = body
      if (postCond(res)) res.success else "Failed postCond".failNel
    } catch {
      case e => e.getMessage.failNel
    }
  }

  def validate[T](body: => T): ValidationNEL[String, T] = {

    test(body)({_: T => true})
  }

  class NPOption[T](val o: Option[T]) {
    def getOrFail(msg: String): ValidationNEL[String, T] = {
      o match {
        case Some(v) => v.success
        case _ => msg.failNel
      }
    }
  }

  case class VL(loc: String)
  implicit def VLtoString(vl: VL) = vl.loc

  type ValMsgs = Map[String, String]

  class NPValidationNEL[T](val v: Validation[NonEmptyList[String], T]) {


    def logTxt(src: String, msgs: ValMsgs) =
      "\t" + src + "\t" + msgs.mkString(", ") + "\t" + v.fold(e => e.list.mkString(", "), s => "") + "\t"

    def getOrThrow(prefix: String = "getOrThrow") = v.fold(e => throw new Exception(prefix + ": " + e), s => s)

    def getOrThrow = v.fold(e => throw new Exception("Errors: " + e.list.mkString(", ")), s => s)

    def trace(msgs: ValMsgs)(implicit src: VL): Validation[NonEmptyList[String], T] = {
      if (v.isFailure) Logger.trace("TRACE" + logTxt(src, msgs))
      v
    }

    def debug(msgs: ValMsgs)(implicit src: VL): Validation[NonEmptyList[String], T] = {
      if (v.isFailure) Logger.debug("DEBUG" + logTxt(src, msgs))
      v
    }

    def info(msgs: ValMsgs)(implicit src: VL): Validation[NonEmptyList[String], T] = {
      if (v.isFailure) Logger.info("INFO" + logTxt(src, msgs))
      v
    }

    def warn(msgs: ValMsgs)(implicit src: VL): Validation[NonEmptyList[String], T] = {
      if (v.isFailure) Logger.warn("WARN" + logTxt(src, msgs))
      v
    }

    def error(msgs: ValMsgs)(implicit src: VL): Validation[NonEmptyList[String], T] = {
      if (v.isFailure) Logger.error("ERROR" + logTxt(src, msgs))
      v
    }
  }

  implicit def vToNPValidationNEL[T](v: Validation[String, T]): NPValidationNEL[T] = {
    new NPValidationNEL(v.liftFailNel)
  }

  implicit def nelToValidationNEL[T](np: ValidationNEL[String, T]): NPValidationNEL[T] = {
    new NPValidationNEL(np)
  }
  
  implicit def optToNPOption[T](o: Option[T]): NPOption[T] = {
    new NPOption(o)
  }
}