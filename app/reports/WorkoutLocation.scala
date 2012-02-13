package models
// TODO: Make the above work as package reports - had problems with the page helper expecting a model

import play.api.db._
import play.api.Play.current

import anorm._
import anorm.SqlParser._
import play.api.Logger


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

    val sDate = try{ startDate.toInt } catch { case e => 0; case _ => startDate.toInt }
    val eDate = try{ endDate.toInt }   catch { case e => 99999999; case _ => endDate.toInt }


    DB.withConnection { implicit connection =>

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
          l.club_name,
          count(distinct(s.machine_id)) as screens,
          sum(0) as newReg,
          sum(0) as totReg,
          sum(s.workout_cnt) as woCnt,
          sum(s.workout_regisr) as woReg,
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
      ).as(WorkoutLocation.simple.*)

      Logger.info("tot1: " + tot1.toString)

      val tot2 = SQL(
        """
        select l.club_name, 0 as screens, 
          sum(case 1 when create_day_int >= {sDate} and create_day_int <= {eDate} then 1 else 0 end ) as newReg,
          sum(case 1 when create_day_int <= {eDate} then 1 else 0 end ) as totReg,
          sum(0) as WoCnt, sum(0) as woReg, sum(0) as WoPercReg, sum(0) as woPerScreen, sum(0) as woScreenDay, sum(0) as durAvg, sum(0) as durTot,
          e.location_id
        from exerciser e, location l 
        where l.company_id = {filter}
        and e.location_id = l.location_id
        """
      ) .on(
        'filter -> filter,
        'sDate -> sDate,
        'eDate -> eDate
      ).as(WorkoutLocation.simple.*)

      Logger.info("tot2: " + tot2.toString())

      // There most certainly a more elegant way to do this. But this works and took 2 min. No case clase or new parser required.
      val totalCombined = List(WorkoutLocation(tot1(0).clubName, tot1(0).screens, tot2(0).newReg, tot2(0).totReg,
                                              tot1(0).woCnt, tot1(0).woReg, tot1(0).woPercReg, tot1(0).woPerScreen, tot1(0).woScreenDay, tot1(0).durAvg, tot1(0).durTot ))


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

      Page(woL, totalCombined, page, offset, totalRows)
    }

  }

}
