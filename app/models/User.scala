package models

import play.api.db._
import play.api.Play.current

import anorm._
import anorm.SqlParser._

case class User(id: Pk[Long] = NotAssigned, firstName: String, lastName: String,  email: String)

/**
 * Helper for pagination.
 */
case class Page[A](items: Seq[A], page: Int, offset: Long, total: Long) {
  lazy val prev = Option(page - 1).filter(_ >= 0)
  lazy val next = Option(page + 1).filter(_ => (offset + items.size) < total)
}


object User {

  // -- Parsers

  /**
   * Parse a User from a ResultSet
   */
  val simple = {
    get[Pk[Long]]("user.id") ~
      get[String]("user.first_name") ~
      get[String]("user.last_name") ~
      get[String]("user.email") map {
      case id~firstName~lastName~email => User(id, firstName, lastName, email)
    }
  }

  // -- Queries

  /**
   * Retrieve a user from the id.
   */
  def findById(id: Long): Option[User] = {
    DB.withConnection { implicit connection =>
      SQL("select * from user where id = {id}").on('id -> id).as(User.simple.singleOpt)
    }
  }

  /**
   * Return a page of users.
   *
   * @param page Page to display
   * @param pageSize Number of users per page
   * @param orderBy firstName for sorting
   * @param filter Filter applied on the firstName column
   */
  def list(page: Int = 0, pageSize: Int = 10, orderBy: Int = 1, filter: String = "%"): Page[User] = {

    val offest = pageSize * page

    DB.withConnection { implicit connection =>

      val computers = SQL(
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
      ).as(User.simple *)

      val totalRows = SQL(
        """
          select count(*) from user
          where user.first_name like {filter}
        """
      ).on(
        'filter -> filter
      ).as(scalar[Long].single)

      Page(computers, page, offest, totalRows)

    }

  }

//  /**
//   * Update a computer.
//   *
//   * @param id The computer id
//   * @param computer The computer values.
//   */
//  def update(id: Long, computer: Computer) = {
//    DB.withConnection { implicit connection =>
//      SQL(
//        """
//          update computer
//          set name = {name}, introduced = {introduced}, discontinued = {discontinued}, company_id = {company_id}
//          where id = {id}
//        """
//      ).on(
//        'id -> id,
//        'name -> computer.name,
//        'introduced -> computer.introduced,
//        'discontinued -> computer.discontinued,
//        'company_id -> computer.companyId
//      ).executeUpdate()
//    }
//  }
//
//  /**
//   * Insert a new computer.
//   *
//   * @param computer The computer values.
//   */
//  def insert(computer: Computer) = {
//    DB.withConnection { implicit connection =>
//      SQL(
//        """
//          insert into computer values (
//            (select next value for computer_seq),
//            {name}, {introduced}, {discontinued}, {company_id}
//          )
//        """
//      ).on(
//        'name -> computer.name,
//        'introduced -> computer.introduced,
//        'discontinued -> computer.discontinued,
//        'company_id -> computer.companyId
//      ).executeUpdate()
//    }
//  }
//
//  /**
//   * Delete a computer.
//   *
//   * @param id Id of the computer to delete.
//   */
//  def delete(id: Long) = {
//    DB.withConnection { implicit connection =>
//      SQL("delete from computer where id = {id}").on('id -> id).executeUpdate()
//    }
//  }

}
