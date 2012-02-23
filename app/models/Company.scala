package models

import play.api.db._
import play.api.Play.current

import anorm._
import anorm.SqlParser._

case class Company(id: Pk[Long] = NotAssigned, name: String)

object Company {

  /**
   * Parse a Company from a ResultSet
   */
  val simple = {
    get[Pk[Long]]("location.company_id") ~
      get[String]("location.company_name") map {
      case id~name => Company(id, name)
    }
  }

  /**
   * Construct the Map[String,String] needed to fill a select options set.
   */
  def options: Seq[(String,String)] = DB.withConnection("report") { implicit connection =>
    SQL("select distinct(company_id), company_name from report.location order by company_name").as(Company.simple *).map(c => c.id.toString -> c.name)
  }

}