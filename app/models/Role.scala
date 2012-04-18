package models

import utils._
import play.api.db._
import play.api.Play.current

import anorm._
import anorm.SqlParser._
import play.api.Logger
import org.joda.time._


case class Role(id: Long, name: String, group: Long )
// , createdAt: DateTime, updatedAt: DateTime, createdBy: Long, updatedBy: Long

trait RoleDao {

/* Summary of person, role and rights by target...
  select p.email, ro.name as role, ro.role_group, ri.c, ri.r, ri.u, ri.d, ri.filter, t.name from person p
  join role ro on ro.id = p.role_id
  join rights ri on ri.role_id = ro.id
  join target t on ri.target_id = t.id
  order by email;

*/
  /**
   * Basic parsing of role table.
   *
   */
  val rlSimple = {
    get[Long]("role.id") ~
      get[String]("role.name") ~
      get[Long]("role.role_group") map {
      case id~name~group => Role(id, name, group)
    }
  }


  /**
   * find a role ID by role name
   *
   * @param roleName  the role to find. See Role case class above
   * @return  optional [id, Name, roleGroup] tuple  on success
   */
  def rlFindByName(roleName: String): Option[Role] = {
    implicit val loc = VL("Role.findByName")

    vld {
      DB.withConnection("s2") {
        implicit connection =>
          SQL("""
              select * from role where name = {name}
            """
          ).on('name -> roleName).as(rlSimple.singleOpt)
      }
    }.info.fold(e => None, s => s)
  }

  /** find a role name from its ID
   *
   * @param roleId  the role to find. See Role case class above
   * @return  optional [id, Name, roleGroup] tuple  on success
   */
  def rlFindById(roleId: Long): Option[Role] = {
    implicit val loc = VL("Role.findById")

    vld {
      DB.withConnection("s2") {
        implicit connection =>
          SQL(
            """
              select * from role where id = {id}
            """
          ).on('id -> roleId).as(rlSimple.singleOpt)
      }
    }.info.fold(e => None, s => s)
  }

  /** return the list of roles for a given role_group
   *
   * @param roleId the roleId of the roleGroup to find. See Role case class above
   * @return  seq of Role case class objects
   */
  def rlGroupList(roleId: Long): Seq[Role] = {
    implicit val loc = VL("Role.groupList")

    rlFindById(roleId) match {
      case Some(r) =>
        vld {
          DB.withConnection("s2") {
            implicit connection =>
            // NOTE: returns all role_groups for Netpulse users since they have role_group 1
              var whereStr = if(r.group == 1 ) "" else  " where role_group = {roleGroup} "

              SQL(
                "select * from role" + whereStr
              ).on('roleGroup -> r.group).as (rlSimple *)
          }
        }.info.fold(e => Seq(), s => s)
      case _ => Seq()
    }

  }
}