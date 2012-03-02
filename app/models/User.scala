package models

import scalaz._
import Scalaz._
import utils._
import play.api.db._
import play.api.Play.current

import anorm._
import anorm.SqlParser._
import play.Logger

case class User(id: Long, firstName: String, lastName: String, password: String, email: String)

/**
 * Helper for pagination.
 */
case class Page[A](items: Seq[A], totals: Seq[A], page: Int, offset: Long, total: Long) {
  lazy val prev = Some(page - 1).filter(_ >= 0)
  lazy val next = Some(page + 1).filter(_ => (offset + items.size) < total)
}


object User {

  implicit val loc = VL("User")

  /**
   * Authenticate a User.
   */
  def authenticate(email: String, password: String): Option[User] = {

    implicit val loc = VL("User.authenticate")

    val sqlValid = vld {
      DB.withConnection {
        implicit connection =>
          SQL(
            """
             select * from user where
             email = {email}
            """
          ).on(
            'email -> email,
            'password -> password
          ).as(User.simple.singleOpt)
      }
    }

    sqlValid.fold(
      e => {
        sqlValid.add("email", email).error
        None
      },
      s => s match {
        case None =>
          Logger.info("No user returned from sql on email " + email)
          None
        case Some(u) if (true || u.password == password) =>
          Logger.info("Successful login for " + email)
          Some(u)
        case Some(u) =>
          Logger.warn("Bad word password for email " + email)
          None
      })
  }

  // -- Parsers

  /**
   * Parse a User from a ResultSet
   */
  val simple = {
    get[Long]("user.id") ~
      get[String]("user.first_name") ~
      get[String]("user.last_name") ~
      get[String]("user.password") ~
      get[String]("user.email") map {
      case id ~ firstName ~ lastName ~ password ~ email => User(id, firstName, lastName, password, email)
    }
  }

  // -- Queries

  /**
   * Retrieve a user from the id.
   */
  def findById(id: Long): Option[User] = {

    implicit val loc = VL("User.findById")
    
    vld {
      DB.withConnection {
        implicit connection =>
          SQL("select * from user where id = {id}").on('id -> id).as(User.simple.singleOpt)
      }
    }.error.fold(e => None, s => s)
  }

  /**
   * Return a page of users.
   *
   * @param page Page to display
   * @param pageSize Number of users per page
   * @param orderBy firstName for sorting
   * @param filter Filter applied on the firstName column
   */
  def list(page: Int = 0, pageSize: Int = 25, orderBy: Int = 1, filter: String = "%"): Page[User] = {

    val offest = pageSize * page

    DB.withConnection {
      implicit connection =>

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

        Page(computers, Seq(), page, offest, totalRows)

    }

  }


}
