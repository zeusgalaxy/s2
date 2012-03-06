package models

import utils._
import play.api.db._
import play.api.Play.current

import anorm._
import anorm.SqlParser._
import play.api.Logger

case class Exerciser(dbId: Long, login: String, email: String,
                     vtUserId: String, vtToken: String, vtTokenSecret: String)

/** Anorm-based model representing an exerciser.
 *
 */
object Exerciser {

  implicit val loc = VL("Exerciser")

  /** Basic parsing of an exerciser from the database.
   *
   */
  val simple = {
    get[Long]("exerciser.id") ~
      get[String]("exerciser.login") ~
      get[String]("exerciser.email") ~
      get[String]("exerciser.vt_user_id") ~
      get[String]("exerciser.vt_token") ~
      get[String]("exerciser.vt_token_secret") map {
      case dbId ~ login ~ email ~ vtUserId  ~ vtToken ~ vtTokenSecret =>
        Exerciser(dbId, login, email, vtUserId, vtToken, vtTokenSecret)
    }
  }

  /** Retrieves an exerciser from the database using their numeric id.
   *
   * @param dbId Exerciser's numeric id (as assigned by the db).
   * @return Some(Exerciser), if found; else None.
   */
  def findByDbId(dbId: Long): Option[Exerciser] = {

    implicit val loc = VL("Exerciser.findById")

    vld {
      DB.withConnection {
        implicit connection =>
          SQL("select * from exerciser where id = {id}").on('id -> dbId).as(Exerciser.simple.singleOpt)
      }
    }.info.fold(e => None, s => s)
  }

  /** Retrieves an exerciser from the database using the identifier they log in with.
   *
   * @param login Exerciser's login identifier
   * @return Some(Exerciser), if found; else None.
   */
  def findByLogin(login: String): Option[Exerciser] = {

    implicit val loc = VL("Exerciser.findByLogin")

    vld {
      DB.withConnection {
        implicit connection =>
          SQL("select * from exerciser where login = {login}").on('login -> login).as(Exerciser.simple.singleOpt)
      }
    }.info.fold(e => None, s => s)
  }

  /** Updates the Virtual Trainer fields in the exerciser record with the values provided by the
   * Virtual Trainer api, once we've successfully registered or linked the Netpulse exerciser with
   * Virtual Trainer.
   *
   * @param npLogin The exerciser's login identifier.
   * @param vtUserId The Virtual Trainer user id for this exerciser.
   * @param vtToken The token provided by Virtual Trainer that represents a logged-in session
   * for this exerciser. This token should remain active and valid until explicitly logged out
   * by us (which we never do).
   * @param vtTokenSecret The token secret associated with the token, above.
   * @return True if successful, else False.
   */
  def updVT(npLogin: String, vtUserId: String, vtToken: String, vtTokenSecret: String): Boolean = {

    implicit val loc = VL("Exerciser.updateVirtualTrainer")

    vld {
      DB.withConnection {
        implicit connection =>
          SQL(
            """
              update exerciser
              set vt_user_id = {vtUserId}, vt_token = {vtToken}, vt_token_secret = {vtTokenSecret}
              where login = {login}
            """
          ).on(
            'login -> npLogin,
            'vtUserId -> vtUserId,
            'vtToken -> vtToken,
            'vtTokenSecret -> vtTokenSecret
          ).executeUpdate()
      }
    }.info.fold(e => false, s => true)
  }
}
