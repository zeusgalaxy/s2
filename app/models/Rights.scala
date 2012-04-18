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

trait RightsDao {

  val rtSimple = {
    get[Int] ("rights.c") ~
      get[Int] ("rights.r") ~
      get[Int] ("rights.u") ~
      get[Int] ("rights.d") ~
      get[Int] ("rights.filter") map {
      case c ~ r ~ u ~ d ~ filter => Rights(c, r, u, d, filter)
    }
  }

  def rtGet(u: Option[Person], t: Target): Rights = {
    u match {
      case Some(user) => {
        implicit val loc = VL("Rights.apply")

        vld {
          DB.withConnection("s2") {
            implicit connection =>
              SQL("select c, r, u, d, filter from rights " +
                " join target on rights.target_id = target.id " +
                " join person on rights.role_id = person.role_id " +
                " where person.id = {personId} and target.name = {targetName}").
                on('personId -> user.id,
                'targetName -> t.t).
                as(rtSimple.singleOpt)
          }
        }.info.fold(e => noRights, s => s.getOrElse(noRights))
      }
      case _ => noRights
    }
  }
}
