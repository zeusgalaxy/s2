package models
// TODO: Make the above work as package reports - had problems with the page helper expecting a model

import play.api.db._
import play.api.Play.current
import java.math._

import anorm._
import anorm.SqlParser._
import play.api.Logger


case class WorkoutLocation( clubName: String, screens: Long, newReg: java.math.BigDecimal, totReg: java.math.BigDecimal,
                           woCnt: java.math.BigDecimal, woReg: java.math.BigDecimal, woPercReg: java.math.BigDecimal,
                           woPerScreen: java.math.BigDecimal, woScreenDay: java.math.BigDecimal, durAvg: java.math.BigDecimal,
                           durTot: java.math.BigDecimal
                            )

//case class tot1(  screens: Long, woCnt: java.math.BigDecimal, woReg: java.math.BigDecimal, woPercReg: java.math.BigDecimal,
//                            woPerScreen: java.math.BigDecimal, woScreenDay: java.math.BigDecimal, durAvg: java.math.BigDecimal,
//                            durTot: java.math.BigDecimal
//                            )
//
//case class tot2 (  newReg: java.math.BigDecimal, totReg: java.math.BigDecimal )

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
//    map {
//      case screens~woCnt~woReg~woPercReg~woPerScreen~woScreenDay~durAvg~durTot =>
//        (screens, woCnt, woReg, woPercReg, woPerScreen, woScreenDay, durAvg, durTot )
//    }
  }

  val tot2_parse = {
    get[java.math.BigDecimal]("newReg") ~
      get[java.math.BigDecimal]("totReg")  
//    map {
//      case newReg~totReg =>
//        tot2(newReg, totReg )
//    }
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
   * @param orderBy firstName for sorting
   * @param filter Filter applied on the firstName column
   */
  def list(page: Int = 0, pageSize: Int = pageLength, orderBy: Int = 1, 
           filter: String = "%", startDate: String = "0", endDate: String = "0"): Page[WorkoutLocation] = {

    val offset = pageSize * page

    val sDate = try{ startDate.toInt } catch { case e => 0; case _ => startDate..toInt }
    val eDate = try{ endDate.toInt }   catch { case e => 99999999; case _ => endDate.toInt }


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
          join (select
                  sum(case 1 when create_day_int >= {sDate} and create_day_int <= {eDate} then 1 else 0 end ) as newReg,
                  sum(case 1 when create_day_int <= {eDate} then 1 else 0 end ) as totReg,
                  location_id
                  from exerciser
                  group by location_id) reg on reg.location_id = s.location_id
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
      // There most certainly a more elegant way to do this. But this works and took 2 min. No case clase or new parser required.
//      val totalCombined = List(WorkoutLocation( tot1(0).screens, tot2(0).newReg, tot2(0).totReg,
//                                              tot1(0).woCnt, tot1(0).woReg, tot1(0).woPercReg, tot1(0).woPerScreen, tot1(0).woScreenDay, tot1(0).durAvg, tot1(0).durTot ))


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

  }

}
