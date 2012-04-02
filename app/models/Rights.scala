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


case class Rights(c: Int, r: Int, u: Int, d: Int, f: Int) extends Can {

  override def canCreate = c > 0

  override def canRead = r > 0

  override def canUpdate = u > 0

  override def canDelete = d > 0

  override def isFiltered = f > 0
}

object Rights {

  val simple = {
    get[Int] ("rights.c") ~
      get[Int] ("rights.r") ~
      get[Int] ("rights.u") ~
      get[Int] ("rights.d") ~
      get[Int] ("rights.filter") map {
      case c ~ r ~ u ~ d ~ filter => Rights(c, r, u, d, filter)
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
                'targetName -> t.t).
                as(Rights.simple.singleOpt)
          }
        }.info.fold(e => noRights, s => s.getOrElse(noRights))
      }
      case _ => noRights
    }
  }
}
