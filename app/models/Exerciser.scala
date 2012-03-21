package models

import utils._
import play.api.db._
import play.api.Play.current

import org.joda.time._
import anorm._
import anorm.SqlParser._
import play.api.Logger
import xml._
import scalaz.{Node => _, _}
import Scalaz._

case class Exerciser(dbId: Long, login: String, email: String, pic: Int,
                     membershipId: Option[String], gender: Boolean, dob: DateTime,
                     emailPrefs: Int, weight: Long, homeClubId: Option[Long], homeClubName: Option[String],
                     vtUserId: String, vtToken: String, vtTokenSecret: String, vtStatus: Int) {

  def hasVtConnection = (!vtUserId.isEmpty && !vtToken.isEmpty && !vtTokenSecret.isEmpty)

  def insertVtDetails(x: Node, parent: String) = {
    XmlMutator(x).add(parent,
      <exerciser>
      <email>{email}</email>
      <virtualTrainerStatus>{vtStatus.toString}</virtualTrainerStatus>
      <homeClub>
        <id>{homeClubId.getOrElse("").toString}</id>
        <name>{homeClubName.getOrElse("")}</name>
        </homeClub>
      </exerciser>
    )
  }
}

/** Anorm-based model representing an exerciser.
 *
 */
object Exerciser {

  implicit val loc = VL("Exerciser")

  /** Basic parsing of an exerciser from the database.
   *
   */

  val selectFields = " exerciser.id, exerciser.login, exerciser.email, exerciser.pic, " +
    " exerciser.membership_id, exerciser.gender," +
    " date(exerciser.date_of_birth) as dob, exerciser.email_prefs, exerciser.weight, exerciser.location_id," +
    " exerciser.vt_user_id, exerciser.vt_token, exerciser.vt_token_secret, exerciser.vt_status "

  val simple = {
    get[Long]("exerciser.id") ~
      get[String]("exerciser.login") ~
      get[String]("exerciser.email") ~
      get[Int]("exerciser.pic") ~
      get[Option[String]]("exerciser.membership_id") ~
      get[Boolean]("exerciser.gender") ~
      get[java.util.Date]("dob") ~
      get[Int]("exerciser.email_prefs") ~
      get[Long]("exerciser.weight") ~
      get[Option[Long]]("exerciser.location_id") ~
      get[String]("exerciser.vt_user_id") ~
      get[String]("exerciser.vt_token") ~
      get[String]("exerciser.vt_token_secret") ~
      get[Int]("exerciser.vt_status") ~
      get[Option[String]]("location.name") map {
      case dbId ~ login ~ email ~ pic ~ membershipId ~ gender ~ dob ~ emailPrefs ~
        weight ~ homeClubId ~ vtUserId ~ vtToken ~ vtTokenSecret ~ vtStatus ~ homeClubName =>
        Exerciser(dbId, login, email, pic, membershipId, gender, new DateTime(dob.toString), emailPrefs,
          weight, homeClubId, homeClubName, vtUserId, vtToken, vtTokenSecret, vtStatus)
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
          SQL("select "+selectFields+" , location.name from exerciser left join location on (exerciser.location_id = location.id)" +
            " where id = {id}").on('id -> dbId).as(Exerciser.simple.singleOpt)
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
          SQL("select"+selectFields+" , location.name from exerciser left join location on (exerciser.location_id = location.id)" +
            " where login = {login}").on('login -> login).as(Exerciser.simple.singleOpt)
      }
    }.info.fold(e => None, s => s)
  }

  /** Retrieves exerciser id based on their login. This mapping from login to id occurs frequently, as the client
   * often thinks in terms of the login, whereas the database is mostly structured around id.
   *
   * @param login Exerciser's login identifier
   * @return The exerciser's id as Some(Long), if found; else None.
   */
  def getId(login: String): Option[Long] = {

    implicit val loc = VL("Exerciser.getId")

    vld {
      DB.withConnection {
        implicit connection =>
          SQL("select id from exerciser" +
            " where login = {login}")
          .on('login -> login)
          .as((get[Long]("exerciser.id")).singleOpt)
      }
    }.info.fold(e => None, s => s)
  }

  /** Retrieves any previously saved "favorite channels" for a given exerciser at a given club location.
   *
   * @param login Exerciser's login identifier
   * @param locationId Club where the exerciser is currently at
   * @return List[Long] of channel numbers, if any found; else Nil
   */
  def getSavedChannels(login: String, locationId: Long): List[Long] = {

    implicit val loc = VL("Exerciser.getSavedChannels")

    vld {
      val id = getId(login).getOrFail("Exerciser login " + login + " not found")

      DB.withConnection("S2") {
        implicit connection =>
          SQL("select tv_channel from exerciser_profile join club_exerciser_channel on" +
            " (exerciser_profile.person_id = club_exerciser_channel.exerciser_id and club_exerciser_channel.club_id = {locationId})" +
            " where exerciser_profile.client_login = {login}")
          .on('login -> login,
              'locationId -> locationId)
          .as((get[Long]("club_exerciser_channel.tv_channel"))*)
      }
    }.info.fold(e => Nil, s => s)
  }

  /** Saves "favorite channels" for a given exerciser at a given club location. The list of
   * favorite channels may be empty, which effectively deletes the favorite channels list for that exerciser/club.
   *
   * @param login Exerciser's login identifier
   * @param locationId Club where the exerciser is currently at
   * @param channels List[Long] of channel numbers to save; may be Nil
   * @return true if successful, else false
   */
  def setSavedChannels(login: String, locationId: Long, channels: List[Long]): Boolean = {

    implicit val loc = VL("Exerciser.setSavedChannels")

    vld {

      val id = getId(login).getOrFail("Exerciser login " + login + " not found")
      /**
       * First, clear out any channels that have previously been saved for this exerciser/location.
       */
      DB.withConnection("S2") {
        implicit connection =>
          SQL(
            """
              delete from club_exerciser_channel
              where exerciser_id = {id}
              and club_id = {locationId}
            """
          ).on(
            'id -> id,
            'locationId -> locationId
          ).executeUpdate()
      }

      /**
       * Next, insert each specified channel (if any), one at a time.
       */

      DB.withConnection("S2") {
        implicit connection =>
          channels.foreach{ ch =>
            SQL(
              """
                insert into club_exerciser_channel values({locationId}, {id}, {ch})
              """
            ).on(
              'locationId -> locationId,
              'id -> id,
              'ch -> ch
            ).executeUpdate()
          }
      }

    }.info.fold(e => false, s => true)
  }

  /** Updates the Virtual Trainer fields in the exerciser record with the values provided by the
   * Virtual Trainer api, once we've successfully registered or logged in the Netpulse exerciser with
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
  def loginVt(npLogin: String, vtUserId: String, vtToken: String, vtTokenSecret: String): Boolean = {

    implicit val loc = VL("Exerciser.loginVt")

    vld {
      DB.withConnection {
        implicit connection =>
          SQL(
            """
              update exerciser
              set vt_user_id = {vtUserId}, vt_token = {vtToken}, vt_token_secret = {vtTokenSecret},
                  vt_status = {vtStatus}
              where login = {login}
            """
          ).on(
            'login -> npLogin,
            'vtUserId -> vtUserId,
            'vtToken -> vtToken,
            'vtTokenSecret -> vtTokenSecret,
            'vtStatus -> vtStatusLoggedIn
          ).executeUpdate()
      }
    }.info.fold(e => false, s => true)
  }

  /** Clears the Virtual Trainer token fields in the exerciser record to reflect the fact that they
   * are not currently logged in with VT; although that represents being "unlinked" in the eyes of the
   * user, we still maintain the link with VT under the covers (in the form of still having their
   * user id). The reason we maintain that vt user id is so that we don't attempt to relink the two
   * accounts in future, when they decide to reactivate the connection; we need to know that they
   * have previously been tied together.
   *
   * @param npLogin The exerciser's login identifier.
   * @return True if successful, else False.
   */
  def logoutVt(npLogin: String): Boolean = {

    implicit val loc = VL("Exerciser.logoutVt")

    vld {
      DB.withConnection {
        implicit connection =>
          SQL(
            """
              update exerciser
              set vt_token = "", vt_token_secret = "", vt_status = {vtStatus}
              where login = {login}
            """
          ).on(
            'login -> npLogin,
            'vtStatus -> vtStatusLinked
          ).executeUpdate()
      }
    }.info.fold(e => false, s => true)
  }}
