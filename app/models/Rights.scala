package models

import security._

case class Rights(c: Boolean, r: Boolean, u: Boolean, d: Boolean, f: Boolean) extends Can {
  override def canCreate = c
  override def canRead = r
  override def canUpdate = u
  override def canDelete = d
  override def isFiltered = f
}

object Rights {

  def apply(u: Option[User], t: Target) = {
    u match {
      case Some(user) => {
        // TODO - go to database
        new Rights(c = true, r = true, u = true, d = true, f = false)
      }
      case _ => noRights
    }
  }
}
