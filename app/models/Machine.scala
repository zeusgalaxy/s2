package models

import play.api.db._
import play.api.Play.current

import anorm._
import anorm.SqlParser._

case class MachineBasic(id: Long, locationId: Long, model: String)

object Machine {

  val basic = {
    get[Long]("machine.id") ~
      get[Long]("machine.location_id") ~
      get[String]("machine.model") map {
      case id~locationId~model => MachineBasic(id, locationId, model)
    }
  }

  def getBasic(id: Long): Option[MachineBasic] = {
    DB.withConnection { implicit connection =>
      SQL("select id, location_id, model from machine where id = {id}").on('id -> id).as(Machine.basic.singleOpt)
    }
  }

}