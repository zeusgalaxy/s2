package models

import utils._
import play.api.db._
import play.api.Play.current

import anorm._
import anorm.SqlParser._
import play.Logger

// <adminUser id="89" compId="1" oemId="null" adId="null" email="dfaust@netpulse.com"></adminUser>
//case class User(id: Long, firstName: String, lastName: String, password: String, email: String, compId: Long,  oemId: Long, adId: Long  )
case class User(id: Long, firstName: String, lastName: String, password: String, email: String  )

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

    val sqlValid = validate {
      DB.withConnection {
        implicit connection =>
          SQL(
            """
             select * from admin_user where
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
        sqlValid.error(Map("msg" -> ("Error occurred during sql processing for email " + email)))
        None
      },
      s => s match {
        case None =>
          Logger.info("No user returned from sql on email " + email)
          None
        case Some(u) if ( u.password == Blowfish.encrypt(password) ) =>
          Logger.info("Successful login for " + email)
          Some(u)
        case Some(u) =>
          Logger.warn("Bad word password for email " + email )
          None
      })
  }

  // -- Parsers

  /**
   * Parse a User from a ResultSet
   */
  val simple = {
    get[Long]("user.id") ~
      get[String]("admin_user.first_name") ~
      get[String]("admin_user.last_name") ~
      get[String]("admin_user.password") ~
      get[String]("admin_user.email") map {
          case id ~ firstName ~ lastName ~ password ~ email => User(id, firstName, lastName, password, email)
        }
    //  ~ compId ~ oemId ~ adId
    // , compId, oemId, adId
  }

  // -- Queries

  /**
   * Retrieve a user from the id.
   */
  def findById(id: Long): Option[User] = {

    implicit val loc = VL("User.findById")
    
    validate {
      DB.withConnection {
        implicit connection =>
          SQL("select * from admin_user where id = {id}").on('id -> id).as(User.simple.singleOpt)
      }
    }.error(Map("msg" -> "Error returned from SQL")).fold(e => None, s => s)
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
            select * from admin_user
            where admin_user.first_name like {filter}
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
            select count(*) from admin_user
            where admin_user.first_name like {filter}
          """
        ).on(
          'filter -> filter
        ).as(scalar[Long].single)

        Page(computers, Seq(), page, offest, totalRows)

    }

  }


}
