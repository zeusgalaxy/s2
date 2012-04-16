package models

import play.api.db._
import play.api.Play.current

import anorm._
import anorm.SqlParser._
import utils._

case class Company(id: Long, name: String)

/** Anorm-based model representation of company. The underlying data for the company
 * may come from either a reporting data warehouse or the titan transactional
 * database, depending on the circumstance. Different parsers are provided for the
 * different requirements.
 *
 */
object Company {

  /**
   * Basic parsing of companies table.
   *
   */
  val reportBasic = {
    get[Long]("company.id") ~
      get[String]("company.name") map {
      case id~name => Company(id, name)
    }
  }

  /** Yields valid company ids and names from the location table in the reporting data warehouse
   * for use as options in a pick list.
   *
   * @return List of tuples of company ids and company names.
   */
  def reportCompanyOptions: Seq[(String,String)] = {

    implicit val loc = VL("Company.reportCompanyOptions")

    vld { 
      DB.withConnection("s2") {
        implicit connection =>
          SQL("select id, name from company order by name").
          as(Company.reportBasic *).map(c => c.id.toString -> c.name)
      }
    }.error.fold(e => Seq(), s => s )
  }

}