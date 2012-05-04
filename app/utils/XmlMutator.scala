package utils

import xml.{Elem, Node}
import xml.Elem._
import xml.transform.{RuleTransformer, RewriteRule}

case class XmlMutator(oldXml: Node) {

  private def addChild(n: Node, newChild: Node) = n match {
    case Elem(prefix, label, attribs, scope, child @ _*) =>
      Elem(prefix, label, attribs, scope, child ++ newChild : _*)   // Type mismatches from IntelliJ can be ignored !?
    case _ => sys.error("Can only add children to elements!")
  }

  private class AddChildrenTo(label: String, newChild: Node) extends RewriteRule {
    override def transform(n: Node) = n match {
      case n @ Elem(_, `label`, _, _, _*) => addChild(n, newChild)
      case other => other
    }
  }
  def add(targetElem: String, newChild: Elem): Node =
    new RuleTransformer(new AddChildrenTo(targetElem, newChild)).transform(oldXml).head
//    xml.Utility.trim(new RuleTransformer(new AddChildrenTo(targetElem, newChild)).transform(oldXml).head)
}