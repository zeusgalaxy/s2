package models
// TODO: Make the above work as package reports - had problems with the page helper expecting a model

import play.api.db._
import play.api.Play.current

import anorm._
import anorm.SqlParser._

case class WorkoutLocation(id: Pk[Long] = NotAssigned, firstName: String, lastName: String,  email: String)

/**
 * Helper for pagination.
 */
case class Page[A](items: Seq[A], page: Int, offset: Long, total: Long) {
  lazy val prev = Option(page - 1).filter(_ >= 0)
  lazy val next = Option(page + 1).filter(_ => (offset + items.size) < total)
}


object WorkoutLocation {

  // -- Parsers

  /**
   * Parse a WorkoutLocation from a ResultSet
   */
  val simple = {
    get[Pk[Long]]("User.id") ~
      get[String]("User.first_name") ~
      get[String]("User.last_name") ~
      get[String]("User.email") map {
      case id~firstName~lastName~email => WorkoutLocation(id, firstName, lastName, email)
    }
  }

  // -- Queries

  /**
   * Retrieve a WorkoutLocation from the id.
   */
  def findById(id: Long): Option[WorkoutLocation] = {
    DB.withConnection { implicit connection =>
      SQL("select * from user where id = {id}").on('id -> id).as(WorkoutLocation.simple.singleOpt)
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
  def list(page: Int = 0, pageSize: Int = 25, orderBy: Int = 1, filter: String = "%"): Page[WorkoutLocation] = {

    val offest = pageSize * page

    DB.withConnection { implicit connection =>

      val woL = SQL(
        """
          select * from user
          where user.first_name like {filter}
          order by {orderBy}
          limit {pageSize} offset {offset}
        """
      ).on(
        'pageSize -> pageSize,
        'offset -> offest,
        'filter -> filter,
        'orderBy -> orderBy
      ).as(WorkoutLocation.simple *)

      val totalRows = SQL(
        """
          select count(*) from user
          where user.first_name like {filter}
        """
      ).on(
        'filter -> filter
      ).as(scalar[Long].single)

      Page(woL, page, offest, totalRows)

    }

  }

}
