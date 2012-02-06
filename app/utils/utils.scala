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
  
  class NPValidation[E,S](v: Validation[E,S]) {
    def getOrThrow = v.fold(e=>throw new Exception(e.toString), s=>s)
  }
  implicit def toNPValidation[E,S](v: Validation[E,S]): NPValidation[E,S] = {
    new NPValidation(v)
  }
}