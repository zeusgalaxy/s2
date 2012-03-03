package models

import utils._
import play.api.db._
import play.api.Play.current

import anorm._
import anorm.SqlParser._
import play.Logger
import play.api.mvc.Cookie

// <adminUser id="89" compId="1" oemId="null" adId="null" email="dfaust@netpulse.com"></adminUser>
case class User(id: Long, firstName: String, lastName: String, password: String, email: String, 
                compId: Long,  oemId: Option[Long], adId: Option[Long]  )

/**
 * Helper for pagination.
 */
case class Page[A](items: Seq[A], totals: Seq[A], page: Int, offset: Long, total: Long ) {
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
             email = {email} and password = {password} and status = '1'
            """
          ).on(
            'email -> email,
            'password ->  Blowfish.encrypt (password)
          ).as(User.simple.singleOpt)
      }
    }

    sqlValid.fold(
      e => {
        sqlValid.error(Map("msg" -> ("Error occurred during sql processing for authentication " + email)))
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
      get[String]("admin_user.first_name") ~
      get[String]("admin_user.last_name") ~
      get[String]("admin_user.password") ~
      get[String]("admin_user.email") ~
      get[Long]("admin_user.company_id") ~
      get[Option[Long]]("admin_user.oem_id") ~
      get[Option[Long]]("admin_user.advertiser_id") map {
          case id ~ firstName ~ lastName ~ password ~ email ~ compId ~ oemId ~ adId =>
            User(id, firstName, lastName, password, email, compId, oemId, adId )
        }
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
   * Retrieve a user by their email address 
   */
  def findByEmail(email: String): Option[User] = {

    implicit val loc = VL("User.findEmail")

    validate {
      DB.withConnection {
        implicit connection =>
          SQL("select * from admin_user where email = {email}").on('email -> email).as(User.simple.singleOpt)
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

  /**
   * Create an encrypted admin user cookie to be added onto the session
   * <adminUser id="89" compId="1" oemId="null" adId="null" email="dfaust@netpulse.com"></adminUser>
   */
  def createNpadminCookie(email: String): String = {

    findByEmail(email) match {
      case Some(user) =>
        val xmlStr = new DesEncrypter(DesEncrypter.SESSION_SECRET_KEY).encrypt(
          "<adminUser id=\""+user.id.toString + "\" compId=\""+user.compId+ "\" oemId=\""+user.oemId+
            "\" adId=\""+user.adId+ "\" email=\""+user.email+ "\"></adminUser>" )
        xmlStr
      case _ => ""
    }
  }
  /**
   * Parse the encrypted admin user cookie previously added onto the session
   */
  def parseNpadminCookie (cookie: Option[Cookie]): Option[User] = {
    cookie match {
      case Some(c) => {
        val xmlStr = new DesEncrypter(DesEncrypter.SESSION_SECRET_KEY).decrypt(c.value.replace("\\r", "\r").replace("\\n", "\n"))
        Logger.info( "npadmin cookie = "+c+"\npassed value="+c.value+"\nresult="+xmlStr)
        
        validate {
          // replaces (scala.xml.XML.loadString(xmlStr) \ "@email").toString
          def ls(attr: String, s: String = xmlStr) = (scala.xml.XML.loadString(xmlStr) \ attr).toString

          new User(
            ls("@id").toLong,
            "", "","",
            ls("@email"),
            ls("@compId").toLong,
          // TODO: Come up with a general case for handling "null" values from the DB that will become Longs
            validate { ls("@oemId").toLong }.trace(Map("msg" -> "null db value")).fold(e=> None, s => Some(s) ),
            validate { ls("@adId").toLong }.trace(Map("msg" -> "null db value")).fold(e=> None, s => Some(s) )
          )
        }.error(Map("msg" -> "Error in parsing npadmin cookie from xml")).fold(e => None, u => Some(u))
      }
      case None => None
    }
  }



}
