package models

import utils._
import play.api.db._
import play.api.Play.current

import anorm._
import anorm.SqlParser._
import play.api.Logger

case class Exerciser(dbId: Long, login: String, vtToken: String, vtTokenSecret: String)

object Exerciser {

  val simple = {
    get[Long]("exerciser.id") ~
      get[String]("exerciser.login") ~
      get[String]("exerciser.vt_token") ~
      get[String]("exerciser.vt_token_secret") map {
      case dbId ~ login ~ vtToken ~ vtTokenSecret => Exerciser(dbId, login, vtToken, vtTokenSecret)
    }
  }

  def findByDbId(dbId: Long): Option[Exerciser] = {

    implicit val loc: ValLoc = "Exerciser.findById"

    validate {
      DB.withConnection {
        implicit connection =>
          SQL("select * from exerciser where id = {id}").on('id -> dbId).as(Exerciser.simple.singleOpt)
      }
    }.info(Map("msg" -> "Failure during retrieval")).fold(e => None, s => s)
  }

  def findByLogin(login: String): Option[Exerciser] = {

    implicit val loc: ValLoc = "Exerciser.findByLogin"

    validate {
      DB.withConnection {
        implicit connection =>
          SQL("select * from exerciser where login = {login}").on('login -> login).as(Exerciser.simple.singleOpt)
      }
    }.info(Map("msg" -> "Failure during retrieval")).fold(e => None, s => s)
  }

  def updateToken(login: String, token: String, tokenSecret: String): Boolean = {

    implicit val loc: ValLoc = "Exerciser.updateToken"

    validate {
      DB.withConnection {
        implicit connection =>
          SQL(
            """
              update exerciser
              set vt_token = {vtToken}, vt_token_secret = {vtTokenSecret}
              where login = {login}
            """
          ).on(
            'login -> login,
            'vtToken -> token,
            'vtTokenSecret -> tokenSecret
          ).executeUpdate()
      }
    }.info(Map("msg" -> "Failure during update")).fold(e => false, s => true)
  }

  //
  //  /**
  //   * Insert a new computer.
  //   *
  //   * @param computer The computer values.
  //   */
  //  def insert(computer: Computer) = {
  //    DB.withConnection { implicit connection =>
  //      SQL(
  //        """
  //          insert into computer values (
  //            (select next value for computer_seq),
  //            {name}, {introduced}, {discontinued}, {company_id}
  //          )
  //        """
  //      ).on(
  //        'name -> computer.name,
  //        'introduced -> computer.introduced,
  //        'discontinued -> computer.discontinued,
  //        'company_id -> computer.companyId
  //      ).executeUpdate()
  //    }
  //  }
  //
  //  /**
  //   * Delete a computer.
  //   *
  //   * @param id Id of the computer to delete.
  //   */
  //  def delete(id: Long) = {
  //    DB.withConnection { implicit connection =>
  //      SQL("delete from computer where id = {id}").on('id -> id).executeUpdate()
  //    }
  //  }

}
