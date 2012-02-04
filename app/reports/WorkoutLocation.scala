package models
// TODO: Make the above work as package reports - had problems with the page helper expecting a model

import play.api.db._
import play.api.Play.current

import anorm._
import anorm.SqlParser._

// available columns : location.company_id, location.location_id, location.company_name, location.club_name,
// location.club_city, location.club_zip,
// workout_day_sum.machine_id, workout_day_sum.location_id, workout_day_sum.day_int, workout_day_sum.duration_avg,
// workout_day_sum.duration_tot, workout_day_sum.duration_pct, workout_day_sum.workout_cnt,
// workout_day_sum.e, workout_day_sum.workout_uniq_user, workout_day_sum.calories_avg,
// workout_day_sum.calories_tot, workout_day_sum.distance_avg, workout_day_sum.distance_tot,
// workout_day_sum.distance_unit, workout_day_sum.speed_avg, workout_day_sum.speed_min, workout_day_sum.speed_max,
// workout_day_sum.speed_unit, workout_day_sum.vertical_avg, workout_day_sum.vertical_unit,
// workout_day_sum.used_wkt_program_pct,
// time_dim.day_int, time_dim.year_number, time_dim.month_number, time_dim.day_of_year_number,
// time_dim.day_of_month_number, time_dim.day_of_week_number, time_dim.week_of_year_number, time_dim.day_name,
// time_dim.month_name, time_dim.quarter_number, time_dim.quarter_name, time_dim.year_quarter_name,
// time_dim.weekend_ind, time_dim.days_in_month_qty, time_dim.day_int_1, time_dim.day_desc, time_dim.week_sk,
// time_dim.day_date, time_dim.week_name, time_dim.week_of_month_number, time_dim.week_of_month_name,
// time_dim.year_sk, time_dim.month_sk, time_dim.quarter_sk, time_dim.day_of_week_sort_name, time_dim.year_sort_number

case class WorkoutLocation(id: Pk[Long] = NotAssigned, clubName: String,
                           screens: Long, workoutCnt: java.math.BigDecimal
                            )

object WorkoutLocation {

  val companyLimit = "Life Time Fitness"
  val pageLength = 25

  // -- Parsers

  /**
   * Parse a WorkoutLocation from a ResultSet
   */
  val simple = {
    get[Pk[Long]]("location.location_id") ~
      get[String]("location.club_name") ~
      get[Long]("screens") ~
      get[java.math.BigDecimal]("wo_cnt") map {
      case id~clubName~screens~workoutCnt =>
        WorkoutLocation(id, clubName, screens, workoutCnt )
    }
  }

  // -- Queries

  /**
   * Retrieve a WorkoutLocation from the id.
   */
  def findById(id: Long): Option[WorkoutLocation] = {
    DB.withConnection { implicit connection =>
      SQL("select * from tmp_workout_report twr where id = {id} and company_name = {companyLimit}").on(
        'id -> id, 'companyLimit -> companyLimit).as(WorkoutLocation.simple.singleOpt)
    }
  }

  /**
   * Return a page of WorkoutLocations.
   *
   * @param page Page to display
   * @param pageSize Number of WorkoutLocations per page
   * @param orderBy firstName for sorting
   * @param filter Filter applied on the firstName column
   */
  def list(page: Int = 0, pageSize: Int = pageLength, orderBy: Int = 1, filter: String = "%"): Page[WorkoutLocation] = {

    val offset = pageSize * page

    DB.withConnection { implicit connection =>

      val woL = SQL(
        """
          select l.location_id, l.club_name,
          count(distinct(s.machine_id)) as screens,
          sum(s.workout_cnt) as wo_cnt,
          sum(s.workout_regisr) as reg_wo,
          sum(s.workout_cnt) / sum(s.workout_regisr) as perc_reg,
          count(distinct(s.machine_id)) / sum(s.workout_cnt) as wo_screen,
          count(distinct(s.day_int)) as days,
          sum(s.workout_cnt) / count(distinct(s.day_int)) as wo_screen_day,
          sum(s.duration_tot) / sum(s.workout_cnt) as dur_avg,
          sum(s.duration_tot) / 60 as dur_tot,
          count(*)
          from  workout_day_sum s
          join location l on s.location_id = l.location_id
          join time_dim t on t.day_int = s.day_int
          where company_name = {companyLimit}
          group by l.location_id
          order by {orderBy}
          limit {pageSize} offset {offset}
        """
      ).on(
        'pageSize -> pageSize,
        'offset -> offset,
        'filter -> filter,
        'companyLimit -> WorkoutLocation.companyLimit,
        'orderBy -> orderBy
      ).as(WorkoutLocation.simple *)

      val totalRows = SQL(
        """
          select count(*) from  workout_day_sum s
          join location l on s.location_id = l.location_id
          join time_dim t on t.day_int = s.day_int
          where club_name like {filter}
          and company_name = {companyLimit}
        """
      ).on(
        'filter -> filter,
        'companyLimit -> WorkoutLocation.companyLimit
      ).as(scalar[Long].single)

      Page(woL, page, offset, totalRows)

    }

  }

}
