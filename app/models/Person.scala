package models

import utils._
import play.api.db._
import play.api.Play.current

import org.joda.time._
import anorm._
import anorm.SqlParser._
import play.api.Logger
import scalaz.{Node => _, _}

case class Person(id: Int, firstName: String, lastName: String, portalLogin: String, portalPassword: Option[String],
                    email: String, phone: String, lastLogin: DateTime, activeStatus: Int)

/**
 * Anorm-based model representing any person having a relationship to Netpulse.
 */
object Person {

  implicit val loc = VL("Person")

  /**
   * Basic parsing of a person from the database.
   */

  val selectFields = " person.id, person.first_name, person.last_name, person.portal_login, person.portal_password, " +
                      " person.email, person.phone, date(person.last_login_dt) as lastLogin, person.active_status "

  val simple = {
    get[Int]("person.id") ~
      get[String]("person.first_name") ~
      get[String]("person.last_name") ~
      get[String]("person.portal_login") ~
      get[Option[String]]("person.portal_password") ~
      get[String]("person.email") ~
      get[String]("person.phone") ~
      get[java.util.Date]("lastLogin") ~
      get[Int]("person.active_status") map {
      case id ~ firstName ~ lastName ~ portalLogin ~ portalPassword ~ email ~ phone ~ lastLogin ~ activeStatus =>
        Person(id, firstName, lastName, portalLogin, portalPassword, email, phone, new DateTime(lastLogin.toString), activeStatus)
    }
  }

  /** Retrieves a person from the database using their numeric id.
   *
   * @param id Person's numeric id (as assigned by the db).
   * @return Some(Person), if found; else None.
   */
  def findById(id: Int): Option[Person] = {

    implicit val loc = VL("Person.findById")

    vld {
      DB.withConnection("s2") {
        implicit connection =>
          SQL("select "+selectFields+" from person " +
            " where id = {id}").on('id -> id).as(Person.simple.singleOpt)
      }
    }.info.fold(e => None, s => s)
  }

  /** Retrieves a person from the database using their portal login identifier.
   *
   * @param login Person's login identifier used when accessing the portal
   * @return Some(Person), if found; else None
   */
  def findByLogin(login: String): Option[Person] = {

    implicit val loc = VL("Person.findByEmail")

    vld {
      DB.withConnection("s2") {
        implicit connection =>
          SQL("select "+selectFields+" from person " +
            " where portal_login = {login}").on('login -> login).as(Person.simple.singleOpt)
      }
    }.info.fold(e => None, s => s)
  }
}
