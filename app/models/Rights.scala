package models

import security._

import utils._
import play.api.db._
import play.api.Play.current

import anorm._
import anorm.SqlParser._
import play.api.Logger
import scalaz._
import Scalaz._


case class Rights(c: Boolean, r: Boolean, u: Boolean, d: Boolean, f: Boolean) extends Can {

  override def canCreate = c

  override def canRead = r

  override def canUpdate = u

  override def canDelete = d

  override def isFiltered = f
}

object Rights {

  val simple = {
    get[Short] ("rights.c") ~
      get[Short] ("rights.r") ~
      get[Short] ("rights.u") ~
      get[Short] ("rights.d") ~
      get[Short] ("rights.filter") map {
      case c ~ r ~ u ~ d ~ filter =>
        Rights ((c > 0) ? true | false, (r > 0) ? true | false , (u > 0) ? true | false,
                (d > 0) ? true | false, (filter > 0) ? true | false)
    }
  }

  def apply(u: Option[Person], t: Target): Rights = {
    u match {
      case Some(user) => {
        implicit val loc = VL("Rights.apply")

        vld {
          DB.withConnection("s2") {
            implicit connection =>
              SQL("select c, r, u, d, filter from rights " +
                " join target on rights.target_id = target.id " +
                " join person_role on rights.role_id = person_role.role_id " +
                " where person_role.person_id = {personId} and target.name = {targetName}").
                on('personId -> user.id,
                'targetName -> t).
                as(Rights.simple.singleOpt)
          }
        }.info.fold(e => noRights, s => s.getOrElse(noRights))
      }
      case _ => noRights
    }
  }
}
