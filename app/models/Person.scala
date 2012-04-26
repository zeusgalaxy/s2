package models

import utils._
import play.api.db._
import play.api.Play.current

import org.joda.time._
import anorm._
import anorm.SqlParser._
import play.api.Logger
import scalaz.{Node => _, _}
import Scalaz._

case class Person(id: Long, companyId: Option[Long], roleId: Long, firstName: String, lastName: String,
                  portalLogin: String, portalPassword: Option[String],
                  email: String, phone: String, activeStatus: Int) {

  def isActive = activeStatus == 1

}
/**
 * Anorm-based model representing any person having a relationship to Netpulse.
 */
trait PersonDao {

  val prPageLength = 15

  /**
   * Basic parsing of a person from the database.
   */

  lazy val prSelectFields = " person.id, person.company_id, person.role_id, person.first_name, person.last_name, person.portal_login, person.portal_password, " +
    " person.email, person.phone, date(person.last_login_dt) as lastLogin, person.active_status "

  lazy val prSimple = {
    get[Long]("person.id") ~
      get[Option[Long]]("person.company_id") ~
      get[Long]("person.role_id") ~
      get[String]("person.first_name") ~
      get[String]("person.last_name") ~
      get[String]("person.portal_login") ~
      get[Option[String]]("person.portal_password") ~
      get[String]("person.email") ~
      get[String]("person.phone") ~
      get[Option[java.util.Date]]("lastLogin") ~
      get[Int]("person.active_status") map {
      case id ~ companyId ~ roleId ~ firstName ~ lastName ~ portalLogin ~ portalPassword ~ email ~ phone ~ lastLogin ~ activeStatus =>
        Person(id, companyId, roleId, firstName, lastName, portalLogin, portalPassword, email, phone, activeStatus)
          // lastLogin.map(ll => Some(new DateTime(ll.toString))).getOrElse(None), activeStatus)
    }
  }

  /**Retrieves a person from the database using their numeric id.
   *
   * @param id Person's numeric id (as assigned by the db).
   * @return Some(Person), if found; else None.
   */
  def prFindById(id: Long): Option[Person] = {

    implicit val loc = VL("PersonDao.prFindById")

    vld {
      DB.withConnection("s2") {
        implicit connection =>
          SQL("select " + prSelectFields + " from person where id = {id} ").
            on('id -> id).as(prSimple.singleOpt)
      }
    }.logInfo.fold(e => None, s => s)
  }

  /**Retrieves a person from the database using their portal login identifier.
   *
   * @param login Person's login identifier used when accessing the portal
   * @return Some(Person), if found; else None
   */
  def prFindByLogin(login: String): Option[Person] = {

    implicit val loc = VL("PersonDao.prFindByLogin")

    vld {
      DB.withConnection("s2") {
        implicit connection =>
          SQL("select " + prSelectFields + " from person " +
            " where portal_login = {login} ").
            on('login -> login).as(prSimple.singleOpt)
      }
    }.logInfo.fold(e => {println (e.toString()); None}, s => s)
  }

  /**
   * Authenticate a person based on their login (i.e., email) and password
   *
   * @param login Value person uses to login (which is normally their eamil address)
   * @param password, Person's portal password, unencrypted
   * @return Some(Person) if the person found in the db and the password matches, None otherwise.
   */
  def prAuthenticate(login: String, password: String): Option[Person] = {

    implicit val loc = VL("PersonDao.prAuthenticate")

    vld {
      DB.withConnection("s2") {
        implicit connection =>
          SQL(
            "select " + prSelectFields + " from person where " +
              "portal_login = {login} and portal_password = {password} and active_status = 1"
          ).on(
            'login -> login,
            'password -> Blowfish.encrypt(password)
          ).as(prSimple.singleOpt)
      }
    }.logError.fold(e => None, s => s match {
      case None =>
        Logger.info("No (active) match found in db for login " + login + " and password " + password + " in Person.authenticate")
        None
      case u => u        // TODO: Update last login here.
    })
  }

  /**
   * Return a page of Person rows.
   *
   * @param page Page to display
   * @param pageSize Number of users per page
   * @param orderBy firstName for sorting
   * @param userFilter Filter applied on the firstName column
   * @return List of users to display a page with
   */
  def prList(page: Int = 0, pageSize: Int = 15, orderBy: Int = 1, userFilter: String = "%", companyFilter: String): Page[Person] = {

    implicit val loc = VL("PersonDao.prList")

    val offset = pageSize * page
    Logger.debug("orderBy = "+orderBy.toString)
    vld {
      DB.withConnection("s2") {
        implicit connection =>

          val companyWhere = if (companyFilter.isEmpty) "" else " and company_id = " + companyFilter + " "

          val p = SQL(
            "select " + prSelectFields + " from person " +
              "where ifnull(last_name,'') like {filter} and active_status = 1 " + companyWhere +
              " order by {orderBy} limit {pageSize} offset {offset} "
          ).on(
            'pageSize -> pageSize,
            'offset -> offset,
            'filter -> userFilter,
            'orderBy -> orderBy
          ).as(prSimple *)

          val totalRows = SQL(
            "select count(*) from person where ifnull(last_name,'') like {filter} and active_status = 1 "
          ).on(
            'filter -> userFilter
          ).as(scalar[Long].single)

          Page(p, Seq(), page, offset, totalRows)
      }
    }.logError.fold(e => Page(Seq(), Seq(), 0, 0, 0), s => s)
  }

  /**
   * Insert a new Person.
   *
   * @param person The person values.
   * @param createdBy the person ID of the user making this change.
   *@return Optional Long ID
   */
  def prInsert(person: Person, createdBy: Long): Option[Long] = {

    implicit val loc = VL("PersonDao.prInsert")

    val result = vld {
      DB.withConnection("s2") {
        implicit connection => {
          SQL(
            """
              insert into person set company_id={companyId}, role_id={roleId},
               first_name={firstName}, last_name={lastName},
                portal_login = {portalLogin}, portal_password = {portalPassword},
                email = {email}, phone={phone}, active_status = 1, status_dt = NOW(), created_at = NOW(),
                created_by={createdBy}

            """
          ).on(
            'companyId      -> person.companyId,
            'roleId         -> person.roleId,
            'firstName      -> person.firstName,
            'lastName       -> person.lastName,
            'portalLogin    -> person.portalLogin,
            'portalPassword -> Blowfish.encrypt(person.portalPassword.getOrElse("")),
            'email          -> person.email,
            'phone          -> person.phone,
            'createdBy      -> createdBy
          ).executeInsert()
        }
      }
    }.logError.fold(e => None, s => s)
    Logger.debug("Inserted ID : " + result)
    result //  you can println your vld left side (with the error part) by calling the "either" method to turn it into an Either and access it as a "left"
  }


  /**
   * Update a person.
   *
   * Password not encrypted here. Decrypt it only when needed.
   *
   * @param id The person id
   * @param person, The person values from the Person class.
   * @param updatedBy, The person ID of the user updating this record.
   * @return Number of rows updated
   */
  def prUpdate(id: Long, person: Person, updatedBy: Long): Int = {

    implicit val loc = VL("PersonDao.prUpdate")

    val result = vld {

      DB.withConnection("s2") { implicit connection =>
        SQL(
          """
            update person
              set company_id={companyId}, role_id={roleId},
              first_name = {firstName}, last_name = {lastName},
              portal_login = {portalLogin}, email={email}, phone={phone},
              updated_at=NOW(), updated_by={updatedBy} """ +
              person.portalPassword.map{ portalPassword => ", portal_password={portalPassword} "}.getOrElse("")  + """
              where id = {id}
            """
        ).on(
          'id             -> id,
          'companyId      -> person.companyId,
          'roleId         -> person.roleId,
          'firstName      -> person.firstName,
          'lastName       -> person.lastName,
          'portalLogin    -> person.portalLogin,
          'portalPassword -> person.portalPassword.map { portalPassword => Blowfish.encrypt(portalPassword) },
          'email          -> person.email,
          'phone          -> person.phone,
          'updatedBy      -> updatedBy
        ).executeUpdate()
      }
    }.logError.fold(e => 0, s => s)
    Logger.debug("update :"+result)
    result
  }

  /**
   * Delete person permanently and completely
   *
   * @param id Id of the person to delete.
   * @param updatedBy person ID of the user performing this operation
   * @return Number of rows affected - should be 1
   */
  def prDelete(id: Long, updatedBy: Long): Int = {

    implicit val loc = VL("PersonDao.prDelete")

    vld {
      DB.withConnection("s2") {
        implicit connection => {
          Logger.warn("User ID: "+id.toString+" deleted permanently by person id: "+updatedBy)
          SQL("delete from person where id = {id}").
            on('id -> id).executeUpdate()
        }
      }
    }.logError.fold(e => 0, s => s)
  }

}
