package models

import play.api.db._
import play.api.Play.current

import anorm._
import utils._

case class Status()

/**
 * DAO for checking the status of the application
 * add any checks here
 */
trait StatusDao {

  def checkAppStatus: Boolean = checkDB("s2") && checkDB("report") && checkDB()

  def checkDB( db : String = "default" ): Boolean = {
    implicit val loc = VL("status.checkDB")

    vld {
        DB.withConnection(db) { implicit connection => SQL("Select 1").execute() }
    }.logError.fold(e => false, s => s )
  }

}
