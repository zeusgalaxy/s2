package models

import utils._
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
   * Parse a User from a ResultSet
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
   * Retrieve a user from the id.
   */
  def findById(id: Long): Option[User] = {

    implicit val loc = VL("User.findById")

    vld {
      DB.withConnection {
        implicit connection =>
          SQL("select * from admin_user where id = {id}").on('id -> id).as(User.simple.singleOpt)
      }
    }.error.fold(e => None, s => s)
  }

  /**
   * Retrieve a user by their email address 
   */
  def findByEmail(email: String): Option[User] = {

    implicit val loc = VL("User.findEmail")

    vld {
      DB.withConnection {
        implicit connection =>
          SQL("select * from admin_user where email = {email}").on('email -> email).as(User.simple.singleOpt)
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
  def list(page: Int = 0, pageSize: Int = 15, orderBy: Int = 1, filter: String = "%"): Page[User] = {
    
    implicit val loc = VL("User.list")
    
    val offset = pageSize * page

    vld {
      DB.withConnection {
        implicit connection =>

          val u = SQL(
            """
              select * from admin_user
              where oem_id = 1 AND ifnull(last_name,'') like {filter}
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

Logger.debug("User list = "+u.toString)

          Page(u, Seq(), page, offset, totalRows)
      }
    }.error.fold(e => Page(Seq(),Seq(),0,0,0), s => s)
  }

  /**
   * Create an encrypted admin user cookie to be added onto the session
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
   * Parse the encrypted admin user cookie previously added onto the session
   *
   *     npCookieString =>
   *   User.parseNpadminCookie(Cookie("npadmin",npCookieString,0,"",None,true,false)) match {
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
