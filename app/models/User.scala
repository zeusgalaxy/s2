package models

import utils._
import scalaz._
import play.api.db._
import play.api.Play.current

import anorm._
import anorm.SqlParser._
import play.Logger
import play.api.mvc.Cookie


// <adminUser id="89" compId="1" oemId="null" adId="null" email="dfaust@netpulse.com"></adminUser>
case class User(id: Long = 0, firstName: Option[String], lastName: Option[String],
                password: String = "", email: String = "", compId: Option[Long] = None,
                oemId: Option[Long] = None, adId: Option[Long] = None)

/**
 * Helper for pagination.
 */
case class Page[A](items: Seq[A], totals: Seq[A], page: Int, offset: Long, total: Long) {
  lazy val prev = Some(page - 1).filter(_ >= 0)
  lazy val next = Some(page + 1).filter(_ => (offset + items.size) < total)
}

/**Anorm-based model representing an admin user.
 *
 */
object User {

  implicit val loc = VL("User")

  val pageLength = 15

  /**
   * Authenticate a User based on email and password
   *
   * @param email user email
   * @param password, unencrypted user password
   * @return Some(User) if the email and encrypted password are found in the DB, None otherwise.
   */
  def authenticate(email: String, password: String): Option[User] = {

    implicit val loc = VL("User.authenticate")

    val sqlValid = vld {
      DB.withConnection {
        implicit connection =>
          SQL(
            """
             select * from admin_user where
             email = {email} and password = {password} and status = '1'
            """
          ).on(
            'email -> email,
            'password -> Blowfish.encrypt(password)
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
          Logger.info("No valid user returned from sql on email " + email)
          None
        case Some(u) => Some(u)
      })
  }

  // -- Parsers

  /**
   * Parse a User from a SQL ResultSet
   */
  val simple = {
    get[Long]("admin_user.id") ~
      get[Option[String]]("admin_user.first_name") ~
      get[Option[String]]("admin_user.last_name") ~
      get[String]("admin_user.password") ~
      get[String]("admin_user.email") ~
      get[Option[Long]]("admin_user.company_id") ~
      get[Option[Long]]("admin_user.oem_id") ~
      get[Option[Long]]("admin_user.advertiser_id") map {
      case id ~ firstName ~ lastName ~ password ~ email ~ compId ~ oemId ~ adId =>
        User(id, firstName, lastName, password, email, compId, oemId, adId)
    }
  }

  // -- Queries

  /**
   * Retrieve a user by ID from the DB.
   * @param id of the user
   * @return Some(User) if the user is found, None if not.
   */
  def findById(id: Long): Option[User] = {

    implicit val loc = VL("User.findById")

    vld {
      DB.withConnection {
        implicit connection =>
          SQL("select * from admin_user where id = {id} and status = 1").on('id -> id).as(User.simple.singleOpt)
      }
    }.error.fold(e => None, s => s)
  }

  /**
   * Retrieve a user by their email address from the DB
   * @param email string
   * @return Some(User) if user found. None it not.
   */
  def findByEmail(email: String): Option[User] = {

    implicit val loc = VL("User.findEmail")

    val result = vld {
      DB.withConnection {
        implicit connection =>
          SQL("select * from admin_user where email = {email} and status = 1").on('email -> email).as(User.simple.singleOpt)
      }
    }.error.fold(e => None, s => s)
    result
  }

  // -- DB Updates


  /**
   * Return a page of users.
   *
   * @param page Page to display
   * @param pageSize Number of users per page
   * @param orderBy firstName for sorting
   * @param filter Filter applied on the firstName column
   * @return a list of users to display a page with
   */
  def list(page: Int = 0, pageSize: Int = 15, orderBy: Int = 1, filter: String = "%"): Page[User] = {

    implicit val loc = VL("User.list")

    val offset = pageSize * page

    vld {
      DB.withConnection {
        implicit connection =>

          val u = SQL(
            """
              select * from admin_user
              where oem_id = 1 AND ifnull(last_name,'') like {filter} AND status = 1
              order by {orderBy}
              limit {pageSize} offset {offset}
            """
          ).on(
            'pageSize -> pageSize,
            'offset -> offset,
            'filter -> filter,
            'orderBy -> orderBy
          ).as(User.simple *)

          val totalRows = SQL(
            """
              select count(*) from admin_user
              where oem_id =1 and ifnull(last_name,'') like {filter}
            """
          ).on(
            'filter -> filter
          ).as(scalar[Long].single)

          Logger.debug("User list = " + u.toString)

          Page(u, Seq(), page, offset, totalRows)
      }
    }.error.fold(e => Page(Seq(), Seq(), 0, 0, 0), s => s)
  }

  /**
   * Update a user.
   *
   * Password not encrypted here. Decrypt it only when needed.
   *
   * @param id The user id
   * @param user, The user values.
   * @return int the number of rows updated
   */
    def update(id: Long, user: User) = {


      // TODO: NOT TESTED OR VALIDATED IN ANY WAY. WAS PULLED FROM SPRINT


      implicit val loc = VL("User.update")

      val result = vld {
        DB.withConnection { implicit connection =>
          SQL(
            """
              update user
              set name = {name}, introduced = {introduced}, discontinued = {discontinued}, company_id = {company_id}
              where id = {id}
            """
          ).on(
            'id -> id,
            'firstName -> user.firstName,
            'lastName -> user.lastName,
            'password -> user.password,
            'email -> user.email,
            'compId -> user.compId,
            'oemId -> user.oemId,
            'adId -> user.adId
          ).executeUpdate()
        }
      }.error.fold(e => None, s => s)
      Logger.debug("update :"+result)
      result
    }

  /**
   * Insert a new User.
   *
   * @param user The user values.
   * @return Optional Long ID
   */
  def insert(user: User): Option[Long] = {

    implicit val loc = VL("User.insert")

    val result = vld {
      DB.withConnection {
        implicit connection =>
          SQL(
            """
              insert into admin_user values ( 0, {compId}, {oemId}, {adId}, {email}, {firstName}, {lastName},
                "", {password}, NOW(), NULL, 1
              )
            """
          ).on(
            'firstName -> user.firstName,
            'lastName -> user.lastName,
            'password -> Blowfish.encrypt(user.password),
            'email -> user.email,
            'compId -> user.compId,
            'oemId -> user.oemId,
            'adId -> user.adId
          ).executeInsert()
        }
    }.error.fold(e => None, s => s)
    Logger.debug("Insert :"+result)
    result       //  you can println your vld left side (with the error part) by calling the "either" method to turn it into an Either and access it as a "left"
  }

  /**
   * Delete a user by setting their status to 3.
   *
   * @param id Id of the computer to delete.
   * @return int, number of rows affected - should be 1
   */
  def delete(id: Long) = {

    implicit val loc = VL("User.delete")

    vld {
      DB.withConnection {
        implicit connection =>
          SQL("update admin_user set status = 3 where id = {id}").
            on('id -> id).executeUpdate()
        }
     }.error.fold(e => None, s => s)
  }


  /**
   * Create an encrypted admin user cookie to be added onto the session
   *
   * This is for interoperability with Dino. Pass this to Dino (and write some code on that side)
   * if you want to send an existing S2 session to Dino.
   * <adminUser id="89" compId="1" oemId="null" adId="null" email="dfaust@netpulse.com"></adminUser>
   */
  def createNpadminCookie(email: String): String = {

    findByEmail(email) match {
      case Some(user) =>
        val xmlStr = new DesEncrypter(DesEncrypter.SESSION_SECRET_KEY).encrypt(
          "<adminUser id=\"" + user.id.toString + "\" compId=\"" + user.compId + "\" oemId=\"" + user.oemId +
            "\" adId=\"" + user.adId + "\" email=\"" + user.email + "\"></adminUser>")
        xmlStr
      case _ => ""
    }
  }

  /**
   * Parse the encrypted admin user cookie previously added onto the session by Dino
   *  Usage:
   *     npCookieString =>
   *   User.parseNpadminCookie(Cookie("npadmin",npCookieString,0,"",None,true,false)) match {
   * @param c, a 3DES encrypted xml string with adminUser data in it. Example above
   * @return User case class with xml string attributes parsed and placed into it. None on failure.
   */
  def parseNpadminCookie(c: Cookie): Option[User] = {

    implicit val loc = VL("User.parseNpadmin")

    val xmlStr = new DesEncrypter(DesEncrypter.SESSION_SECRET_KEY).decrypt(c.value.replace("\\r", "\r").replace("\\n", "\n"))
    Logger.info("npadmin cookie = " + c + "\npassed value=" + c.value + "\nresult=" + xmlStr)

    vld {
      // replaces (scala.xml.XML.loadString(xmlStr) \ "@email").toString
      def ls(attr: String, s: String = xmlStr) = (scala.xml.XML.loadString(xmlStr) \ attr).toString

      new User(
        ls("@id").toLong,
        None, None, "",
        ls("@email"),
        Some(ls("@compId").toLong),
        // TODO: Come up with a general case for handling "null" values from the DB that will become Longs
        vld {
          ls("@oemId").toLong
        }.trace.fold(e => None, s => Some(s)),
        vld {
          ls("@adId").toLong
        }.trace.fold(e => None, s => Some(s))
      )
    }.error.fold(e => None, u => Some(u))
  }


}
