package models

import utils._
import play.api.db._
import play.api.Play.current

import org.joda.time._
import anorm._
import anorm.SqlParser._
import play.api.Logger
import scalaz.{Node => _, _}

case class Person(id: Long, companyId: Long, roleId: Long, firstName: String, lastName: String,
                  portalLogin: String, portalPassword: Option[String],
                  email: String, phone: String)
// lastLogin: Option[DateTime], activeStatus: Int)

/**
 * Anorm-based model representing any person having a relationship to Netpulse.
 */
object Person {

  implicit val loc = VL("Person")

  /**
   * Basic parsing of a person from the database.
   */

  val selectFields = " person.id, person.company_id, person.role_id, person.first_name, person.last_name, person.portal_login, person.portal_password, " +
    " person.email, person.phone, date(person.last_login_dt) as lastLogin, person.active_status "

  val standardWhere = " and active_status = 1 and company_id IS NOT NULL "

  val simple = {
    get[Long]("person.id") ~
      get[Long]("person.company_id") ~
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
        Person( id, companyId, roleId, firstName, lastName, portalLogin, portalPassword, email, phone)
          // lastLogin.map(ll => Some(new DateTime(ll.toString))).getOrElse(None), activeStatus)
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
            " where id = {id} and company_id IS NOT NULL").on('id -> id).as(Person.simple.singleOpt)
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
            " where portal_login = {login} " + standardWhere ).on('login -> login).as(Person.simple.singleOpt)
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
              "portal_login = {login} and portal_password = {password} " + standardWhere
          ).on(
            'login -> login,
            'password -> Blowfish.encrypt(password)
          ).as(Person.simple.singleOpt)
      }
    }.error.fold(e => None, s => s match {
      case None =>
        Logger.info("No match found in db for login " + login + " and password " + password + " in Person.authenticate")
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
   * @param filter Filter applied on the firstName column
   * @return a list of users to display a page with
   */
  def list(page: Int = 0, pageSize: Int = 15, orderBy: Int = 1, userFilter: String = "%", companyFilter: String): Page[Person] = {

    implicit val loc = VL("Person.list")

    val offset = pageSize * page
    Logger.debug("orderBy = "+orderBy.toString)
    vld {
      DB.withConnection("s2") {
        implicit connection =>

          val companyWhere = if (companyFilter.isEmpty) "" else " and company_id = "+companyFilter+" "

          val p = SQL(
            "select " + selectFields + " from person " +
              "where ifnull(last_name,'') like {filter} " + standardWhere +  companyWhere +
              " order by {orderBy} limit {pageSize} offset {offset} "
          ).on(
            'pageSize -> pageSize,
            'offset -> offset,
            'filter -> userFilter,
            'orderBy -> orderBy
          ).as(Person.simple *)

          val totalRows = SQL(
            "select count(*) from person where ifnull(last_name,'') like {filter} "+ standardWhere
          ).on(
            'filter -> userFilter
          ).as(scalar[Long].single)

          Page(p, Seq(), page, offset, totalRows)
      }
    }.error.fold(e => Page(Seq(), Seq(), 0, 0, 0), s => s)
  }


  /**
   * Insert a new Person.
   *
   * @param person The person values.
   * @param companyId Override the company id in the person object. This is passed in here so the controller
   *                  can decide if the user should be allowed to add users not in their company.
   * @param createdBy the person ID of the user making this change.
   *@return Optional Long ID
   */
  def insert(person: Person, companyId: Long, createdBy: Long): Option[Long] = {

    implicit val loc = VL("Person.insert")

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
            'companyId      -> companyId,
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
   * @param person, The person values from the Person class.
   * @param companyId Allows the caller (controller) to override the companyId coming from the edit form for
   *                  security reasons.
   * @param roleId override the roleId for security reasons.
   * @param updatedBy, The person ID of the user updating this record.
   * @return int the number of rows updated
   */
  def update(id: Long, person: Person, companyId: Long, roleId: Long,  updatedBy: Long) = {

    implicit val loc = VL("Person.update")

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
          'companyId      -> (if (companyId != -1) companyId else person.companyId),
          'roleId         -> (if (roleId != -1 ) roleId else person.roleId),
          'firstName      -> person.firstName,
          'lastName       -> person.lastName,
          'portalLogin    -> person.portalLogin,
          'portalPassword -> person.portalPassword.map { portalPassword => Blowfish.encrypt(portalPassword) },
          'email          -> person.email,
          'phone          -> person.phone,
          'updatedBy      -> updatedBy
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
  def delete(id: Long, updatedBy: Long) = {

    implicit val loc = VL("Person.delete")

    vld {
      DB.withConnection("s2") {
        implicit connection =>
          SQL("update person set active_status = 3, status_dt=NOW(), updated_at=now(), updated_by={updatedBy} where id = {id}").
            on('id -> id, 'updatedBy -> updatedBy).executeUpdate()
      }
    }.error.fold(e => None, s => s)
  }

  /**
   * Delete person permanently and completely - see delete also
   *
   * @param id Id of the person to delete.
   * @param updatedBy person ID of the user performing this operation
   * @return int, number of rows affected - should be 1
   */
  def hardDelete(id: Long, updatedBy: Long) = {

    implicit val loc = VL("Person.hardDelete")

    vld {
      DB.withConnection("s2") {
        implicit connection => {
          Logger.warn("User ID: "+id.toString+" deleted permanently by person id: "+updatedBy)
          SQL("delete from person where id = {id}").
            on('id -> id).executeUpdate()
        }
      }
    }.error.fold(e => None, s => s)
  }

}
