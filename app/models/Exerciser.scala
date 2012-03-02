package models

import utils._
import play.api.db._
import play.api.Play.current

import anorm._
import anorm.SqlParser._
import play.api.Logger

case class Exerciser(dbId: Long, login: String, email: String,
                     vtUserId: String, vtToken: String, vtTokenSecret: String)

object Exerciser {

  implicit val loc = VL("Exerciser")

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

  def findByDbId(dbId: Long): Option[Exerciser] = {

    implicit val loc = VL("Exerciser.findById")

    validate {
      DB.withConnection {
        implicit connection =>
          SQL("select * from exerciser where id = {id}").on('id -> dbId).as(Exerciser.simple.singleOpt)
      }
    }.info.fold(e => None, s => s)
  }

  def findByLogin(login: String): Option[Exerciser] = {

    implicit val loc = VL("Exerciser.findByLogin")

    validate {
      DB.withConnection {
        implicit connection =>
          SQL("select * from exerciser where login = {login}").on('login -> login).as(Exerciser.simple.singleOpt)
      }
    }.info.fold(e => None, s => s)
  }

  def updateVirtualTrainer(npLogin: String, vtUserId: String, vtToken: String, vtTokenSecret: String): Boolean = {

    implicit val loc = VL("Exerciser.updateVirtualTrainer")

    validate {
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
