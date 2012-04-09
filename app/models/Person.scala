package models

import utils._
import play.api.db._
import play.api.Play.current

import org.joda.time._
import anorm._
import anorm.SqlParser._
import play.api.Logger
import scalaz.{Node => _, _}

case class Person(id: Long, firstName: String, lastName: String, portalLogin: String, portalPassword: Option[String],
                  email: String, phone: String, lastLogin: DateTime, activeStatus: Int)

/**PersonEdit Class
 * Subset of the Person class that is displayable and editable for using in edit and add forms.
 * @param firstName  fn
 * @param lastName   ln
 * @param portalLogin login string, usually email
 * @param portalPassword  login pw
 * @param email  email
 */
case class PersonEdit(firstName: Option[String], lastName: Option[String], portalLogin: String, portalPassword: Option[String], email: String) {

  def toPerson: Person = Person(id = -1, firstName = firstName.map(p => p).getOrElse(""), lastName = lastName.map(p => p).getOrElse(""),
    portalPassword = portalPassword, email = email, phone = "",
    lastLogin = (new DateTime()), activeStatus = 1, portalLogin = portalLogin)
}

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
    get[Long]("person.id") ~
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

  /**Retrieves a person from the database using their numeric id.
   *
   * @param id Person's numeric id (as assigned by the db).
   * @return Some(Person), if found; else None.
   */
  def findById(id: Long): Option[Person] = {

    implicit val loc = VL("Person.findById")

    vld {
      DB.withConnection("s2") {
        implicit connection =>
          SQL("select " + selectFields + " from person " +
            " where id = {id}").on('id -> id).as(Person.simple.singleOpt)
      }
    }.info.fold(e => None, s => s)
  }

  /**Retrieves a person from the database using their portal login identifier.
   *
   * @param login Person's login identifier used when accessing the portal
   * @return Some(Person), if found; else None
   */
  def findByLogin(login: String): Option[Person] = {

    implicit val loc = VL("Person.findByEmail")

    vld {
      DB.withConnection("s2") {
        implicit connection =>
          SQL("select " + selectFields + " from person " +
            " where portal_login = {login}").on('login -> login).as(Person.simple.singleOpt)
      }
    }.info.fold(e => None, s => s)
  }

  /**
   * Authenticate a person based on their login (i.e., email) and password
   *
   * @param login Value person uses to login (which is normally their eamil address)
   * @param password, Person's portal password, unencrypted
   * @return Some(Person) if the person found in the db and the password matches, None otherwise.
   */
  def authenticate(login: String, password: String): Option[Person] = {

    implicit val loc = VL("Person.authenticate")

    vld {
      DB.withConnection("s2") {
        implicit connection =>
          SQL(
            "select " + selectFields + " from person where " +
              "portal_login = {login} and portal_password = {password} and active_status = 1"
          ).on(
            'login -> login,
            'password -> Blowfish.encrypt(password)
          ).as(Person.simple.singleOpt)
      }
    }.error.fold(e => None, s => s match {
      case None =>
        Logger.info("No match found in db for login " + login + " and password " + password + " in Person.authenticate")
        None
      case u => u
    })
  }

  /**
   * Return a page of Person rows.
   *
   * @param page Page to display
   * @param pageSize Number of users per page
   * @param orderBy firstName for sorting
   * @param filter Filter applied on the firstName column
   * @return a list of users to display a page with
   */
  def list(page: Int = 0, pageSize: Int = 15, orderBy: Int = 1, filter: String = "%"): Page[Person] = {

    implicit val loc = VL("Person.list")

    val offset = pageSize * page

    vld {
      DB.withConnection("s2") {
        implicit connection =>

          val p = SQL(
            "select " + selectFields + " from person " +
              "where ifnull(last_name,'') like {filter} AND active_status = 1 " +
              " order by {orderBy} limit {pageSize} offset {offset} "
          ).on(
            'pageSize -> pageSize,
            'offset -> offset,
            'filter -> filter,
            'orderBy -> orderBy
          ).as(Person.simple *)

          val totalRows = SQL(
            """
              select count(*) from person
              where ifnull(last_name,'') like {filter} AND active_status = 1
            """
          ).on(
            'filter -> filter
          ).as(scalar[Long].single)

          Page(p, Seq(), page, offset, totalRows)
      }
    }.error.fold(e => Page(Seq(), Seq(), 0, 0, 0), s => s)
  }


  /**
   * Insert a new Person.
   *
   * @param person The person values.
   * @return Optional Long ID
   */
  def insert(person: Person): Option[Long] = {

    implicit val loc = VL("Person.insert")

    val result = vld {
      DB.withConnection("s2") {
        implicit connection => {
          SQL(
            """
              insert into person values ( 0, {firstName}, {lastName}, {portalLogin}, {portalPassword},
                {email}, {phone}, NOW(), 1, NOW(), NOW(), NOW(), 1, NULL, NULL  )
            """
          ).on(
            'firstName      -> person.firstName,
            'lastName       -> person.lastName,
            'portalLogin    -> person.portalLogin,
            'portalPassword -> Blowfish.encrypt(person.portalPassword.map(s => s).getOrElse("")),
            'email          -> person.email,
            'phone          -> person.phone
          ).executeInsert()
        }
      }
    }.error.fold(e => None, s => s)
    Logger.debug("Inserted ID : " + result)
    result //  you can println your vld left side (with the error part) by calling the "either" method to turn it into an Either and access it as a "left"
  }


  /**
   * Update a person.
   *
   * Password not encrypted here. Decrypt it only when needed.
   *
   * @param id The person id
   * @param person, The person values from the PersonEdit class NOT the Person class.
   * @return int the number of rows updated
   */
  def update(id: Long, person: PersonEdit) = {

    implicit val loc = VL("Person.update")

    val result = vld {

      DB.withConnection("s2") { implicit connection =>
        SQL(
          """
            update person
              set first_name = {firstName}, last_name = {lastName}, portal_login = {portalLogin}, email={email}, updated_at=NOW() """ +
              person.portalPassword.map{ portalPassword => ", portal_password={portalPassword} "}.getOrElse("")  + """
              where id = {id}
            """
        ).on(
          'id             -> id,
          'firstName      -> person.firstName,
          'lastName       -> person.lastName,
          'portalLogin    -> person.portalLogin,
          'portalPassword -> person.portalPassword.map { portalPassword => Blowfish.encrypt(portalPassword) },
          'email          -> person.email
        ).executeUpdate()
      }
    }.error.fold(e => None, s => s)
    Logger.debug("update :"+result)
    result
  }

  /**
   * Delete a person by setting their status to 3.
   *
   * @param id Id to delete.
   * @return int, number of rows affected - should be 1
   */
  def delete(id: Long) = {

    implicit val loc = VL("Person.delete")

    vld {
      DB.withConnection("s2") {
        implicit connection =>
          SQL("update person set active_status = 3 where id = {id}").
            on('id -> id).executeUpdate()
      }
    }.error.fold(e => None, s => s)
  }

  /**
   * Delete person permanently and completely - see delete also
   *
   * @param id Id of the computer to delete.
   * @return int, number of rows affected - should be 1
   */
  def hardDelete(id: Long) = {

    implicit val loc = VL("Person.hardDelete")

    vld {
      DB.withConnection("s2") {
        implicit connection =>
          SQL("delete from person where id = {id}").
            on('id -> id).executeUpdate()
      }
    }.error.fold(e => None, s => s)
  }

}
