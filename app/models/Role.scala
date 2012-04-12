package models

import utils._
import play.api.db._
import play.api.Play.current

import anorm._
import anorm.SqlParser._
import play.api.Logger
import org.joda.time._


case class Role(id: Long, name: String, Group: Long, createdAt: DateTime, updatedAt: DateTime, createdBy: Long, updatedBy: Long)

object Role {

/* Summary of person, role and rights by target...
  select p.email, ro.name as role, ro.role_group, ri.c, ri.r, ri.u, ri.d, ri.filter, t.name from person p
  join role ro on ro.id = p.role_id
  join rights ri on ri.role_id = ro.id
  join target t on ri.target_id = t.id;

   c.name,     join company c on c.id = p.company_id
*/

  /** find a role ID by role name
   *
   * @param roleName  the role to find. See Role case class above
   * @return  optional [id, Name, roleGroup] tuple  on success
   */
  def findByName(roleName: String): Option[(Long, String, Long)] = {
    implicit val loc = VL("Role.findByName")

    vld {
      DB.withConnection("s2") {
        implicit connection =>
          SQL("""
              select id, name, role_group from role where name = {name}
            """
          ).on('name -> roleName).as( (get[Long]("id")~get[String]("name")~get[Long]("role_group")).map(flatten).singleOpt)
      }
    }.info.fold(e => None, s => s)
  }

  /** find a role name from its ID
   *
   * @param roleId  the role to find. See Role case class above
   * @return  optional [id, Name, roleGroup] tuple  on success
   */
  def findById(roleId: Long): Option[(Long, String, Long)] = {
    implicit val loc = VL("Role.findByName")

    vld {
      DB.withConnection("s2") {
        implicit connection =>
          SQL(
            """
              select id, name, role_group from role where id = {id}
            """
          ).on('id -> roleId).as( (get[Long]("id")~get[String]("name")~get[Long]("role_group")).map(flatten).singleOpt)
      }
    }.info.fold(e => None, s => s)
  }

  /** return the list of roles for a given role_group
   *
   * @param roleGroup  the roleGroup to find. See Role case class above
   * @return  list of optional [id, Name, roleGroup] tuples  on success
   */
  def groupList(roleGroup: Long): Seq[(Long, String)] = {
    implicit val loc = VL("Role.groupList")

    vld {
      DB.withConnection("s2") {
        implicit connection =>
          // NOTE: returns all role_groups for Netpulse users since they have role_group 1
          SQL("select id, name from role " + ( if(roleGroup == 1 ) "" else  " where role_group = {roleGroup} ")

          ).on('roleGroup -> roleGroup).as( (get[Long]("id")~get[String]("name")).map(flatten) *)
      }
    }.info.fold(e => Seq(), s => s)
  }
}