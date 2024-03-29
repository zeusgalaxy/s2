package models

import utils._
import play.api.db._
import play.api.Play.current

import anorm._
import anorm.SqlParser._

case class MachineBasic(id: Long, locationId: Long, model: String)

case class Equipment(id: Long, mfr: Int, model: Int, eType: Option[String],
                     mfrName: Option[String], modelName: Option[String])

/**
 * Anorm-based model representing machines.
 */
trait MachineDao {
  this: MachineDao with EquipmentDao =>

  /**
   * Basic parsing of machine records from the database.
   */
  lazy val mchBasic = {
    get[Long]("machine.id") ~
      get[Long]("machine.location_id") ~
      get[String]("machine.model") map {
      case id ~ locationId ~ model => MachineBasic(id, locationId, model)
    }
  }

  /**
   * Parsing of basic machine values along with more extensive equipment values associated
   * with that machine.
   */
  lazy val mchWithEquip = mchBasic ~ (eqFull ?) map {
    case machine ~ equipment => (machine, equipment)
  }

  /** Retrieves basic machine object based on the machine's database id.
   *
   * @param id Machine identifier.
   * @return Some(MachineBasic) if successful; otherwise None.
   */
  def mchGetBasic(id: Long): Option[MachineBasic] = {

    implicit val loc = VL("Machine.getBasic")

    vld {
      DB.withConnection {
        implicit connection =>
          SQL("select * from machine where id = {id}").on('id -> id).as(mchBasic.singleOpt)
      }
    }.logInfo.fold(e => None, s => s)
  }

  /** Retrieves basic machine object along with its associated equipment information, if available.
   *
   * @param id Machine id.
   * @return Some tuple of MachineBasic and Some Equipment, if available; else None where not available
   * of unsuccessful.
   */
  def mchGetWithEquip(id: Long): Option[(MachineBasic, Option[Equipment])] = {

    implicit val loc = VL("Machine.getWithEquip")

    vld {
      DB.withConnection {
        implicit connection =>
          SQL("select * from machine m join equipment e on m.equipment_id = e.id where m.id = {id}").
            on('id -> id).as(mchWithEquip.singleOpt)
      }
    }.logInfo.fold(e => None, s => s)
  }
}

/**
 * Anorm-based model representing equipment.
 */
trait EquipmentDao {

  lazy val eqFull = {
    long("equipment.id") ~
      int("equipment.mfr") ~
      int("equipment.model") ~
      get[Option[String]]("equipment.type") ~
      get[Option[String]]("equipment.mfr_name") ~
      get[Option[String]]("equipment.model_name") map {
      case id ~ mfr ~ model ~ eType ~ mfrName ~ modelName => Equipment(id, mfr, model, eType, mfrName, modelName)
    }
  }
}
