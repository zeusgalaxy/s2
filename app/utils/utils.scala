/**
 * Package object with misc. utility functions, et. al.
 */

import java.io.StringWriter
import scala.xml._
import play.api._
import play.api.mvc._
import play.api.libs.ws.WS._
import scalaz.{Node => _, Logger => _, _}
import Scalaz._

package object utils {

  def switchHosts(s: String): String = s.replaceFirst("localhost:9000", "qa-v1.netpulse.ws").replaceFirst("ec2", "v1").replaceFirst("v2", "v1")

  def toWSRequest(r: Request[AnyContent]): (WSRequestHolder, Option[Array[Byte]]) = {

    val newBody: Option[Array[Byte]] = r.body match {
      case c @ AnyContentAsFormUrlEncoded(data) => None
      case c @ AnyContentAsText(txt) => Some(txt.getBytes)
      case c @ AnyContentAsXml(xml) => Some(xml.text.getBytes)
      case c @ AnyContentAsJson(json) => Some(json.toString().getBytes)
      case c @ AnyContentAsMultipartFormData(mfd) => None
      case c @ AnyContentAsRaw(raw) => raw.asBytes()
      case _ => None
    }

    val qs = {for (k <- r.queryString.keys; v <- r.queryString.get(k).get) yield (k -> v)}.toMap
    (WSRequestHolder("http://qa-v1.netpulse.ws" + r.uri, Map("ACCEPT-CHARSET" -> Seq("utf-8")), qs, None, None), newBody)
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

    val s = """(.*?)?(<"""+tag+""".*?>)(.*?)(</"""+tag+""">)?(.*)?"""
    val X = s.r
    val X(p1, t1, p2, t2, p3) = xml
    p1 + p2 + p3
  }

  def writeXml(root: Node) = {

    val writer: StringWriter = new StringWriter()
    XML.write(writer, root, "utf-8", true, null)
    writer.toString
  }

  def validate[T](body: => T): Validation[String,T] = {
    
    try {
      body.success
    } catch {
      case e => e.getMessage.fail
    }
  }
  
  class NPValidation[T](v: Validation[String, T]) {
    
    def logTxt(src: String, msg: String) = "\t" + src + "\t" + v.either.left.get + "\t" + msg
    def getOrThrow =  v.fold(e => throw new Exception(e), s=>s)

    def trace(src: String, msg: String): Validation[String, T] = {
      if (v.isFailure) Logger.trace("TRACE" + logTxt(src, msg))
      v
    }
    def debug(src: String, msg: String): Validation[String, T] = {
      if (v.isFailure) Logger.debug("DEBUG" + logTxt(src, msg))
      v
    }
    def info(src: String, msg: String): Validation[String, T] = {
      if (v.isFailure) Logger.info("INFO" + logTxt(src, msg))
      v
    }
    def warn(src: String, msg: String): Validation[String, T] = {
      if (v.isFailure) Logger.warn("WARN" + logTxt(src, msg))
      v
    }
    def error(src: String, msg: String): Validation[String, T] = {
      if (v.isFailure) Logger.error("ERROR" + logTxt(src, msg))
      v
    }

  }
  implicit def toNPValidation[T](v: Validation[String, T]): NPValidation[T] = {
    new NPValidation(v)
  }
}