package utils

import scala.xml.{Node, Elem, Group}

/**
 * A path to a Node in a Node tree.
 */
sealed trait NodePath {
  def depth: Int
}

object NodePath {

  final case object Top extends NodePath {
    def depth = 0
    override def toString = "Top"
  }

  final case class Hole(left: List[Node], parent: NodeLoc, right: List[Node]) extends NodePath {
    def depth = parent.path.depth + 1
    override def toString = parent.path.toString + "/#" + left.size
  }
}

/**
 * A location in a Node tree, consisting of a Node and the path to this Node.
 */
sealed case class NodeLoc(node: Node, path: NodePath) {
  import NodePath._

  protected def create(node: Node, path: Hole) = NodeLoc(node, path)

  override def toString = node.label + " at " + path

  /**
   * Check if this node is the top node
   */
  final def isTop = path == Top

  /**
   * Check if this node is not the top node
   */
  final def isChild = path != Top

  /**
   * Check if this node is the first (or only) sibling
   */
  final def isFirst = path match {
    case Top => true
    case Hole(Nil, _, _) => true
    case _ => false
  }

  /**
   * Check if this node is the last (or only) sibling
   */
  final def isLast = path match {
    case Top => true
    case Hole(_ , _, Nil) => true
    case _ => false
  }

  /**
   * Check if this node is a leaf (i.e. has no children)
   */
  final def isLeaf = node.child.isEmpty

  /**
   * Check if this node is a not a leaf (i.e. has children)
   */
  final def isBranch = !isLeaf

  /**
   * Get the left sibling if it exists, otherwise throw a NavigationException
   */
  final def left = path match {
    case Hole(tl :: l, p, r) => create(tl, Hole(l, p, node :: r))
    case _ => throw NavigationException("Cannot go left from "+this)
  }

  /**
   * Get the left sibling if it exists, otherwise None
   */
  final def leftOpt = path match {
    case Hole(tl :: l, p, r) => Some(create(tl, Hole(l, p, node :: r)))
    case _ => None
  }

  /**
   * Get the right sibling if it exists, otherwise throw a NavigationException
   */
  final def right = path match {
    case Hole(l, p, tr :: r) => create(tr, Hole(node :: l, p, r))
    case _ => throw NavigationException("Cannot go right from "+this)
  }

  /**
   * Get the right sibling if it exists, otherwise None
   */
  final def rightOpt = path match {
    case Hole(l, p, tr :: r) => Some(create(tr, Hole(node :: l, p, r)))
    case _ => None
  }

  /**
   * Get the first (leftmost) sibling (or this location)
   */
  final def start: NodeLoc = if(isFirst) this else left.start

  /**
   * Get the last (rightmost) sibling (or this location)
   */
  final def end: NodeLoc = if(isLast) this else right.end

  /**
   * Get the nth child (starting with 0) if it exists, otherwise throw a NavigationException
   */
  final def down(idx: Int) = {
    val ch = node.child
    if(ch.isEmpty) throw NavigationException("Cannot go down from "+this)
    else create(ch.head, Hole(ch.tail.take(idx).reverse.toList, this, ch.tail.drop(idx+1).toList))
  }

  /**
   * Get the nth child (starting with 0) if it exists, otherwise None
   */
  final def downOpt(idx: Int) = {
    val ch = node.child
    if(ch.isEmpty) None
    else Some(create(ch.head, Hole(ch.tail.take(idx).reverse.toList, this, ch.drop(idx+1).toList)))
  }

  /**
   * Get the last child if it exists, otherwise throw a NavigationException
   */
  final def downLast = {
    val ch = node.child
    if(ch.isEmpty) throw NavigationException("Cannot go down from "+this)
    else create(ch.head, Hole(ch.reverse.toList.tail, this, Nil))
  }

  /**
   * Get the last child if it exists, otherwise None
   */
  final def downLastOpt = {
    val ch = node.child
    if(ch.isEmpty) None
    else Some(create(ch.head, Hole(ch.reverse.toList.tail, this, Nil)))
  }

  /**
   * Get the parent (or throw a NavigationException if this is the top node)
   */
  def up = path match {
    case h : Hole =>
      val l = h.left.reverse ++ (node :: h.right)
      NodeLoc(h.parent.node match {
        case e: Elem => e.copy(child = l)
        case _: Group => new Group(l)
      }, h.parent.path)
    case _ => throw NavigationException("Cannot go up from top node")
  }

  /**
   * Get the parent (or None if this is the top node)
   */
  def upOpt = path match {
    case h : Hole => Some(up)
    case _ => None
  }

  /**
   * Get a location in which this node is replaced with the given node
   */
  final def set(n: Node) = NodeLoc(n, path)

  /**
   * Get a location in which this node is replaced with the given node
   */
  final def update(n: Node) = set(n)

  /**
   * Check if children may be added to this node (i.e. it is an Elem or Group)
   */
  final def isContainer = node match {
    case _: Elem => true
    case _: Group => true
    case _ => false
  }

  /**
   * Get a location in which the children of this node are replaced with the given nodes
   * (or throw a NavigationException if isContainer == false)
   */
  final def setChildren(ch: Seq[Node]) = NodeLoc(node match {
    case e: Elem => e.copy(child = ch)
    case Group(_) => Group(ch)
    case _ => throw NavigationException("Cannot replace children of non-container node "+this);
  }, path)

  /**
   * Get the top node of this tree
   */
  final def top: NodeLoc = if(isTop) this else up.top

  /**
   * Insert the given node to the left of this location and return its location
   * (or throw a NavigationException if this location is at the top)
   */
  final def +: (n: Node) = path match {
    case Hole(l, p, r) => NodeLoc(n, Hole(l, p, node :: r))
    case _ => throw NavigationException("Cannot prepend to "+this)
  }

  /**
   * Insert the given node to the right of this location and return its location
   * (or throw a NavigationException if this location is at the top)
   */
  final def :+ (n: Node) = path match {
    case Hole(l, p, r) => NodeLoc(n, Hole(node :: l, p, r))
    case _ => throw NavigationException("Cannot append to "+this)
  }

  /**
   * Insert the given node to the left of this node's children and return the
   * modified version of this location
   * (or throw a NavigationException is isContainer == false)
   */
  final def \+: (n: Node) = setChildren(n +: node.child)

  /**
   * Insert the given node to the right of this node's children and return the
   * modified version of this location
   * (or throw a NavigationException is isContainer == false)
   */
  final def :\+ (n: Node) = setChildren(node.child :+ n)

  /**
   * Delete this node. Return the location to the right if it exists,
   * otherwise left if it exists, otherwise up if it exists,
   * otherwise throw a NavigationException.
   */
  final def delete = path match {
    case Hole(l, p, tr :: r) => NodeLoc(tr, Hole(l, p, r))
    case Hole(tl :: l, p, r) => NodeLoc(tl, Hole(l, p, r))
    case Hole(l, p, r) =>
      val list = l.reverse ++ r
      NodeLoc(p.node match {
        case e: Elem => e.copy(child = list)
        case _: Group => new Group(list)
      }, p.path)
    case _ => throw NavigationException("Cannot delete top node")
  }

  final def transitive(f: NodeLoc => Option[NodeLoc]) = f(this).map(_.transitiveOrSelf(f))

  final def transitiveOrSelf(f: NodeLoc => Option[NodeLoc]): NodeLoc = f(this) match {
    case None => this
    case Some(n) => n.transitiveOrSelf(f)
  }

  /**
   * Return the following node in document order (or None if this is the last node)
   */
  final def followingOpt: Option[NodeLoc] = downOpt(0) orElse rightOutOpt

  private final def rightOutOpt: Option[NodeLoc] = rightOpt orElse upOpt.flatMap(_.rightOutOpt)

  /**
   * Return the preceding node in document order (or None if this is the top node)
   */
  final def precedingOpt: Option[NodeLoc] = leftOpt.map(n => n.downLastTransitiveOpt.getOrElse(n)) orElse upOpt

  private final def downLastTransitiveOpt: Option[NodeLoc] = downLastOpt.flatMap(_.downLastTransitiveOpt)

  /**
   * Compute the distance of this location from the top (with 0 being the top)
   */
  final def depth = path.depth

  final def childAxis = iterate(downOpt(0))(_.rightOpt)

  final def descendantOrSelfAxis: Iterator[NodeLoc] = Iterator.single(this) ++ childAxis.flatMap(_.descendantOrSelfAxis)

  final def descendantAxis = childAxis.flatMap(_.descendantOrSelfAxis)

  final def parentAxis = upOpt.iterator

  final def ancestorOrSelfAxis: Iterator[NodeLoc] = Iterator.single(this) ++ upOpt.iterator.flatMap(_.ancestorOrSelfAxis)

  final def ancestorAxis = upOpt.iterator.flatMap(_.ancestorOrSelfAxis)

  final def followingSiblingAxis = iterate(rightOpt)(_.rightOpt)

  final def precedingSiblingAxis = iterate(leftOpt)(_.leftOpt)

  final def followingAxis = iterate(followingOpt)(_.followingOpt)

  final def precedingAxis = iterate(precedingOpt)(_.precedingOpt)

  final def selfAxis = Iterator.single(this)

  final def \ (Name: String) = for(n @ NodeLoc(Elem(_, Name, _, _, _*), _) <- childAxis) yield n

  final def \\ (Name: String) = for(n @ NodeLoc(Elem(_, Name, _, _, _*), _) <- descendantAxis) yield n

  private final def iterate[T](start: Option[T])(f: T => Option[T]): Iterator[T] = new Iterator[T] {
    private[this] var acc = start
    def hasNext = acc.isDefined
    def next() = acc match {
      case None => throw new NoSuchElementException("next on empty iterator");
      case Some(x) => val res = x ; acc = f(x) ; res
    }
  }
}

object NodeLoc {
  def apply(node: Node): NodeLoc = new CachedTopNodeLoc(node)
}

final class CachedParentNodeLoc(node: Node, path: NodePath.Hole) extends NodeLoc(node, path) {
  import NodePath._

  override def up = path.parent

  override def upOpt = Some(path.parent)

  override protected def create(node: Node, path: Hole) = new CachedParentNodeLoc(node, path)
}

final class CachedTopNodeLoc(node: Node) extends NodeLoc(node, NodePath.Top) {
  import NodePath._

  override def up = throw NavigationException("Cannot go up from top node")

  override def upOpt = None

  override protected def create(node: Node, path: Hole) = new CachedParentNodeLoc(node, path)
}

case class NavigationException(msg: String) extends RuntimeException(msg)