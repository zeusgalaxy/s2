package models

trait Can {
  def canCreate = false
  def canRead = false
  def canUpdate = false
  def canDelete = false
  def isFiltered = false 
}

case class Target(t: String)    // Wrapped so it's usable as an implicit param

case class Rights(c: Boolean, r: Boolean, u: Boolean, d: Boolean, f: Boolean) extends Can {
  override def canCreate = c
  override def canRead = r
  override def canUpdate = u
  override def canDelete = d
  override def isFiltered = f
}

object Rights {

//  def apply(u: Option[User], t: Target, default: Rights = noRights) = {
//    u match {
//      case Some(user) => {
//        // TODO - go to database
//        Rights(c = true, r = true, u = true, d = true, f = false)
//      }
//      case _ => default
//    }
//  }
}
