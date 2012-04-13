package models
// TODO: Make the above work as package reports - had problems with the page helper expecting a model

import utils._
import scalaz._
import play.api.db._
import play.api.Play.current
import java.math._

import anorm._
import anorm.SqlParser._
import play.Logger


case class WorkoutLocation( clubName: String, screens: Long, newReg: java.math.BigDecimal, totReg: java.math.BigDecimal,
                           woCnt: java.math.BigDecimal, woReg: java.math.BigDecimal, woPercReg: java.math.BigDecimal,
                           woPerScreen: java.math.BigDecimal, woScreenDay: java.math.BigDecimal, durAvg: java.math.BigDecimal,
                           durTot: java.math.BigDecimal
                            )

object WorkoutLocation {

  val pageLength = 15

  // -- Parsers

  /**
   * Parse a WorkoutLocation from a ResultSet
   */
  val simple = {
      get[String]("location.club_name") ~
      get[Long]("screens") ~
      get[java.math.BigDecimal]("newReg") ~
      get[java.math.BigDecimal]("totReg") ~
      get[java.math.BigDecimal]("woCnt") ~
      get[java.math.BigDecimal]("woReg") ~
      get[java.math.BigDecimal]("woPercReg") ~
      get[java.math.BigDecimal]("woPerScreen") ~
      get[java.math.BigDecimal]("woScreenDay") ~
      get[java.math.BigDecimal]("durAvg") ~
      get[java.math.BigDecimal]("durTot") map {
      case clubName~screens~newReg~totReg~woCnt~woReg~woPercReg~woPerScreen~woScreenDay~durAvg~durTot =>
        WorkoutLocation(clubName, screens, newReg, totReg, woCnt, woReg, woPercReg, woPerScreen, woScreenDay, durAvg, durTot )
    }
  }
  // All numbers for a total except newReg and totReg
  val tot1_parse = {
      get[Long]("screens") ~
      get[java.math.BigDecimal]("woCnt") ~
      get[java.math.BigDecimal]("woReg") ~
      get[java.math.BigDecimal]("woPercReg") ~
      get[java.math.BigDecimal]("woPerScreen") ~
      get[java.math.BigDecimal]("woScreenDay") ~
      get[java.math.BigDecimal]("durAvg") ~
      get[java.math.BigDecimal]("durTot")
  }

  val tot2_parse = {
    get[java.math.BigDecimal]("newReg") ~
      get[java.math.BigDecimal]("totReg")  
  }

  
  def total ( p1: Any, p2: Any ): WorkoutLocation = {
    p1 match {
      case (screens:Long)~(woCnt:BigDecimal)~(woReg:BigDecimal)~(woPercReg:BigDecimal)~(woPerScreen:BigDecimal)~(woScreenDay:BigDecimal)~(durAvg:BigDecimal)~(durTot:BigDecimal) =>
        p2 match {
          case (newReg:BigDecimal)~(totReg:BigDecimal) => WorkoutLocation ("", screens, newReg, totReg, woCnt, woReg, woPercReg, woPerScreen, woScreenDay, durAvg, durTot )
        }
    }
  }
  
  // -- Queries


  /**
   * Return a page of WorkoutLocations.
   *
   * @param page Page to display
   * @param pageSize Number of WorkoutLocations per page
   * @param orderBy for sorting defaults to first column
   * @param filter Filter applied on the Company column
   * @param startDate starting date in form yyyy-mm-dd that is converted into a date_int
   * @param endDate ending date converted to a date_int
   */
  def list(page: Int = 0, pageSize: Int = pageLength, orderBy: Int = 1,
           filter: String = "%", startDate: String = "0", endDate: String = "0"): Option[Page[WorkoutLocation]] = {

    implicit val loc = VL("WorkoutLocation.list")
    
    val offset = pageSize * page

    val sDate = dateToInt(startDate, 0)
    val eDate = dateToInt(endDate, 99999999 )

    Logger.info("Date range =" + sDate.toString + " to "+eDate.toString)

    vld {

       DB.withConnection("report") { implicit connection =>

          val woL = SQL(
          """
              select l.club_name,
              count(distinct(s.machine_id)) as screens,
              reg.newReg,
              reg.totReg,
              sum(s.workout_cnt) as woCnt,
              sum(s.workout_regisr) as woReg,
              ifnull( (sum(s.workout_regisr) / sum(s.workout_cnt)) * 100 , 0) as woPercReg,
              ifnull(sum(s.workout_cnt) / count(distinct(s.machine_id)), 0) as woPerScreen,
              ifnull(sum(s.workout_cnt) / count(distinct(s.machine_id)) / count(distinct(s.day_int)), 0) as woScreenDay,
              ifnull(sum(s.duration_tot) / sum(s.workout_cnt), 0) as durAvg,
              ifnull(sum(s.duration_tot) / 60, 0) as durTot
              from  workout_day_sum s
              join location l on s.location_id = l.location_id
              join time_dim t on t.day_int = s.day_int
              left outer join (
                select
                  sum(case 1 when create_day_int >= {sDate} and create_day_int <= {eDate} then 1 else 0 end ) as newReg,
                  sum(case 1 when create_day_int <= {eDate} then 1 else 0 end ) as totReg,
                  location_id
                  from exerciser
                  group by location_id
                  ) reg on reg.location_id = s.location_id
              where company_id = {filter}
              and s.day_int between {sDate} and {eDate}
              group by l.location_id
              order by {orderBy}
              limit {pageSize} offset {offset}
            """
          ).on(
          'pageSize -> pageSize,
          'offset -> offset,
          'filter -> filter,
          'sDate -> sDate,
          'eDate -> eDate,
          'orderBy -> orderBy
          ).as(WorkoutLocation.simple.* )

          val tot1 = SQL(
          """
              select
              count(distinct(s.machine_id)) as screens,
              ifnull( sum(s.workout_cnt), 0) as woCnt,
              ifnull( sum(s.workout_regisr), 0) as woReg,
              ifnull( (sum(s.workout_regisr) / sum(s.workout_cnt)) * 100, 0) as woPercReg,
              ifnull(sum(s.workout_cnt) / count(distinct(s.machine_id)), 0) as woPerScreen,
              ifnull(sum(s.workout_cnt) / count(distinct(s.machine_id)) / count(distinct(s.day_int)), 0) as woScreenDay,
              ifnull(sum(s.duration_tot) / sum(s.workout_cnt), 0) as durAvg,
              ifnull(sum(s.duration_tot) / 60, 0) as durTot
              from  workout_day_sum s
              join location l on s.location_id = l.location_id
              join time_dim t on t.day_int = s.day_int
              where company_id = {filter}
              and s.day_int between {sDate} and {eDate}
            """
          ).on(
          'filter -> filter,
          'sDate -> sDate,
          'eDate -> eDate
          ).as(WorkoutLocation.tot1_parse.single)

          Logger.info("tot1: " + tot1.toString)

          val tot2 = SQL(
          """
            select
              ifnull( sum(case 1 when create_day_int >= {sDate} and create_day_int <= {eDate} then 1 else 0 end ), 0) as newReg,
              ifnull( sum(case 1 when create_day_int <= {eDate} then 1 else 0 end ), 0) as totReg
            from exerciser e, location l
            where l.company_id = {filter}
            and e.location_id = l.location_id
            """
          ) .on(
            'filter -> filter,
            'sDate -> sDate,
            'eDate -> eDate
          ).as(WorkoutLocation.tot2_parse.single)

          val comboTotal: WorkoutLocation = total ( tot1, tot2 )

          Logger.info("tot2: " + tot2.toString())

          Logger.info("comboTotal: " + comboTotal.toString)

          val totalRows = SQL(
            """
              select count(distinct(l.location_id))
              from  workout_day_sum s
              join location l on s.location_id = l.location_id
              join time_dim t on t.day_int = s.day_int
              where company_id = {filter}
              and s.day_int >= {sDate} and s.day_int < {eDate}
            """
          ).on(
            'filter -> filter,
            'sDate -> sDate,
            'eDate -> eDate
          ).as(scalar[Long].single)

          Page(woL, Seq(comboTotal), page, offset, totalRows)
      }

    }.error.fold(e => None, page => Some(page) )

  }

}
