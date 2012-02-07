package models

import utils._
import scalaz._
import Scalaz._
import scala.util.control.Exception._
import java.util.{Date}

import play.api.db._
import play.api.Play.current

import anorm._
import anorm.SqlParser._

// TODO: Create an 'evolution for the table changes'

case class PageViewModel(id: Long, machineId: Long, date: Date, pageCounts: Seq[(String, Long)])

object PageViewModel {

  val dateFormat = new java.text.SimpleDateFormat("yyyy-MM-dd") // format.format(new java.util.Date())


  def parseXML(node: scala.xml.NodeSeq): Option[PageViewModel] =
    Option(PageViewModel(
      id = 0L,
      machineId = (node \ "@machine_id").toString().toLong,
      date = dateFormat.parse((node \ "@date").toString()),
      // = NodeSeq(attract, login, mainMenu, workoutHistory, endOfWorkout)
      // = NodeSeq(1, 1, 1, 1, 1)
      pageCounts = for ((p, c) <- (node \\ "page" \\ "@name") zip (node \\ "page" \\ "@count"))
      yield (p.toString(), c.toString().toLong)
    ))

//
//  def verifyData(pvm: PageViewModel): Validation[String, Boolean] = {
//    if (
//      (MachineModel.findById(pvm.machineId) match {
//        case Some(s) => true;
//        case None => false
//      }) &&
//        pvm.pageCounts.length > 0
//    // TODO: Verify date is reasonable
//    ) true
//    else false
//  }

  def verifyData(pvm: PageViewModel): Boolean = {
    if (
      (MachineModel.findById(pvm.machineId) match {
        case Some(s) => true;
        case None => false
      }) &&
        pvm.pageCounts.length > 0
    // TODO: Verify date is reasonable
    ) true
    else false
  }

  // We can't determine if the incoming record is a duplicate. Therefore this is intentionally loose
  // and adds the record with a new timestamp/machineID as the only unique identifiers.
  //
  def insertSQL(pvm: PageViewModel): Int =
    DB.withConnection {
      implicit connection => {
        pvm.pageCounts.foreach(pc => SQL(
          """
          insert into client_page_view_new values ( NULL,
           {pagename}, NOW(), {pagecount}, {machineid}, NOW()
          )
          """
        ).on(
          'pagename -> pc._1,
          'date -> pvm.date,
          'pagecount -> pc._2,
          'machineid -> pvm.machineId
        ).executeUpdate()
        )
        pvm.pageCounts.length // return # records added, any exception caught in controller.
      }
    }

  def insert(node: scala.xml.NodeSeq): Validation[String, Int] = {

    validate {
      parseXML(node) match {
        case Some(pvm) =>
          if (verifyData(pvm)) {
            insertSQL(pvm)
          }
          else 0
        case _ => 0
      }
    }
  }
}

////////////////////////////////////////////
//
// Machine Model is used to
// verify the machine ID
//
case class MachineModel(id: Pk[Long] = NotAssigned, model: String)

object MachineModel {

  val simple = {
    get[Pk[Long]]("machine.id") ~
      get[String]("machine.model") map {
      case id ~ model => MachineModel(id, model)
    }
  }

  def findById(id: Long): Option[MachineModel] = DB.withConnection {
    implicit connection =>
      SQL("select * from machine where id = {id}").on('id -> id).as(MachineModel.simple.singleOpt)
  }
}
