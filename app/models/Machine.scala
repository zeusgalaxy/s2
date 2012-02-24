package models

import utils._
import play.api.db._
import play.api.Play.current

import anorm._
import anorm.SqlParser._

case class MachineBasic(id: Long, locationId: Long, model: String)

case class Equipment(id: Long, mfr: Int, model: Int, eType: Option[String],
                     mfrName: Option[String], modelName: Option[String])

object Machine {

  val basic = {
    get[Long]("machine.id") ~
      get[Long]("machine.location_id") ~
      get[String]("machine.model") map {
      case id ~ locationId ~ model => MachineBasic(id, locationId, model)
    }
  }

  val withEquip = Machine.basic ~ (Equipment.full ?) map {
    case machine ~ equipment => (machine, equipment)
  }

  def getBasic(id: Long): Option[MachineBasic] = {

    implicit val loc = VL("Machine.getBasic")

    validate {
      DB.withConnection {
        implicit connection =>
          SQL("select * from machine where id = {id}").on('id -> id).as(Machine.basic.singleOpt)
      }
    }.info(Map("msg" -> "Failure during retrieval")).fold(e => None, s => s)
  }

  def getWithEquip(id: Long): Option[(MachineBasic, Option[Equipment])] = {

    implicit val loc = VL("Machine.getWithEquip")

    validate {
      DB.withConnection {
        implicit connection =>
          SQL("select * from machine m join equipment e on m.equipment_id = e.id where m.id = {id}").
            on('id -> id).as(Machine.withEquip.singleOpt)
      }
    }.info(Map("msg" -> "Failure during retrieval")).fold(e => None, s => s)
  }
}

object Equipment {

  val full = {
    long("equipment.id") ~
      int("equipment.mfr") ~
      int("equipment.model") ~
      get[Option[String]]("equipment.type") ~
      get[Option[String]]("equipment.mfr_name") ~
      get[Option[String]]("equipment.model_name") map {
      case id ~ mfr ~ model ~ eType ~ mfrName ~ modelName => Equipment(id, mfr, model, eType, mfrName, modelName)
    }
  }
//  val full = {
//    get[Int]("equipment.id") ~
//      get[Int]("equipment.mfr") ~
//      get[Int]("equipment.model") ~
//      get[Option[String]]("equipment.type") ~
//      get[Option[String]]("equipment.mfr_name") ~
//      get[Option[String]]("equipment.model_name") map {
//      case id ~ mfr ~ model ~ eType ~ mfrName ~ modelName => Equipment(id, mfr, model, eType, mfrName, modelName)
//    }
//  }
}
