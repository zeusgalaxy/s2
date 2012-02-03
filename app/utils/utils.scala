/**
 * Package object with misc. utility functions, et. al.
 */

import java.io.StringWriter
import scala.xml._
import play.api._
import http.Writeable
import play.api.mvc._
import play.api.libs.ws.WS._
import scalaz.{Node => _, Logger => _, _}
import Scalaz._
import scala.util.control.Exception._

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
    (WSRequestHolder("http://qa-v1.netpulse.ws" + r.uri, r.headers.toMap, qs, None, None), newBody)
  }
  
  def postOrGetParams(rq: Request[AnyContent], keys: Seq[String]): Map[String, Seq[String]] = {

    val source = rq.body.asFormUrlEncoded match {
      case Some(form) => form
      case None => rq.queryString
    }
    (for (k <- keys; v <- source.get(k)) yield (k -> v)).toMap
  }

  def writeXml(root: Node) = {

    val writer: StringWriter = new StringWriter()
    XML.write(writer, root, "utf-8", true, null)
    writer.toString
  }

//  def arm[T <: java.io.Closeable,R](resource: T)(body: T => R)(handlers: Catch[R]):R = (
//    handlers
//      andFinally (ignoring(classOf[Any]) { resource.close() })
//      apply body(resource)
//  )
//
//  def linePrinter(lnr: java.io.LineNumberReader) = arm(lnr) { lnr =>
//    var lineNumber = 0
//    var lineText = lnr.readLine()
//    while (null != lineText) {
//      lineNumber += 1
//      println("%4d: %s" format (lineNumber, lineText))
//      lineText = lnr.readLine()
//    }
//    lineNumber
//  } _
//
//  // per http://stackoverflow.com/questions/1644813/scala-2-8-control-exception-what-is-the-point
//  // maybe also look at: http://stackoverflow.com/questions/2903481/using-scala-util-control-exception
//  // and (for scalaz): https://gist.github.com/970717
//  val reader = new java.io.LineNumberReader(new java.io.StringReader("some text"))
//  linePrinter(new java.io.LineNumberReader(reader))(noCatch)
//  linePrinter(new java.io.LineNumberReader(reader))(allCatch withApply (_ => 0))
//  linePrinter(new java.io.LineNumberReader(reader))(allCatch withApply (e => e.toString.toInt))
//
//  def valid[A,B](a: A)(body: A => B)(handlers: Catch[B]): B = (
//    handlers
//      apply body(a)
//  )

  def validate[T](body: => T): Validation[String,T] = {
    
    try {
      body.success
    } catch {
      case e => e.getMessage.fail
    }
  }
}