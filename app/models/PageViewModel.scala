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

  def parseXML(node: scala.xml.NodeSeq): ValidationNEL[String, PageViewModel] =

    validate {
      PageViewModel(
        id = 0L,
        machineId = (node \ "@machine_id").toString().toLong,
        date = dateFormat.parse((node \ "@date").toString()),
        pageCounts = for ((p, c) <- (node \\ "page" \\ "@name") zip (node \\ "page" \\ "@count"))
        yield (p.toString(), c.toString().toLong)
      )
    }

  def verifyData(pvm: PageViewModel): ValidationNEL[String, PageViewModel] = {

    def checkMachine(pvm: PageViewModel): Validation[String, PageViewModel] = {
      Machine.getBasic(pvm.machineId) match {
        case Some(_) => pvm.success;
        case None => ("Machine id " + pvm.machineId.toString + " not found in database").fail
      }
    }

    def checkCounts(pvm: PageViewModel): Validation[String, PageViewModel] = {
      if (pvm.pageCounts.length > 0) pvm.success else "Page view counts is zero".fail
    }

    // Note: IntelliJ's red underline below the checkCounts call appears to be bogus
    (checkMachine(pvm).liftFailNel |@| checkCounts(pvm).liftFailNel) {
      case (_, _) => pvm
    }
  }

  // We can't determine if the incoming record is a duplicate. Therefore this is intentionally loose
  // and adds the record with a new timestamp/machineID as the only unique identifiers.
  //
  def insertSQL(pvm: PageViewModel): ValidationNEL[String, Int] = {

    validate {
      DB.withConnection {
        implicit connection => {
          pvm.pageCounts.foreach {
            pc => SQL(
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
          }
          pvm.pageCounts.length // return # records added, any exception caught in controller.
        }
      }:Int
    }
  }

  def insert(node: scala.xml.NodeSeq): ValidationNEL[String, Int] = {

    for {
      a <- parseXML(node)
      b <- verifyData(a)
      c <- insertSQL(b)
    } yield c
  }
}

