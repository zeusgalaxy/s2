package models

import utils._
import play.api.db._
import play.api.Play.current

import anorm._
import anorm.SqlParser._
import play.api.Logger
import org.joda.time._


case class PersonRole(id: Long, createdAt: DateTime, updatedAt: DateTime, createdBy: Long, updatedBy: Long, personId: Long, roleId: Long)

object PersonRole {

//  val simple = {
//    get[Long]("person_role.id") ~
//      get[java.util.Date]("person_role.created_at") ~
//      get[java.util.Date]("person_role.updated_at") ~
//      get[Long]("person_role.created_by") ~
//      get[Long]("person_role.updated_by") ~
//      get[Long]("person_role.person_id") ~
//      get[Long]("person_role.role_id") map {
//      case id ~ createdAt ~ updatedAt ~ createdBy ~ updatedBy ~ personId ~ roleId =>
//        PersonRole(id, new DateTime(createdAt.toString), new DateTime(updatedAt.toString), createdBy, updatedBy, personId, roleId)
//    }
//  }

  /**Retrieves a PersonRole using the person's numeric id.
   *
   * @param id Person's numeric id (as assigned by the db).
   * @return Some(Role(id)), if found; else None.
   */
  def findByPersonId(id: Long): Option[String] = {

    implicit val loc = VL("PersonRole.findByPersonId")

    vld {
      DB.withConnection("s2") {
        implicit connection =>
          SQL("select role.name from role, person_role where role.id = person_role.role_id and person_id = {id}").on('id -> id).as(scalar[String].singleOpt)
      }
    }.info.fold(e => None, s => s)
  }

  /**insert a new connection between a person and a role
   *
   * @param personId person id
   * @param roleId  ID of the role to connect the person to.
   * @return Optional id:Long depending on success
   */
  def insert(personId: Long, roleId: Long, createdBy: Long = 0): Option[Long] = {
    implicit val loc = VL ("PersonRole.insert")

    val result = vld {
      DB.withConnection ("s2") {
        implicit connection => {
          SQL (
          """
            insert into person_role values ( 0, now(), 0, {createdBy}, 0, {personId}, {roleId} )
          """
          ).on (
          'createdBy  -> createdBy,
          'personId   -> personId,
          'roleId     -> roleId
          ).executeInsert ()
        }
      }
    }.error.fold (e => None, s => s)
    Logger.debug ("Inserted person_role ID : " + result)
    result //  you can println your vld left side (with the error part) by calling the "either" method to turn it into an Either and access it as a "left"
  }

  /**Delete the person_role rows for the given person ID
   *
   * @param id  Id of the person who's roles we want removed
   * @return int, number of rows affected or None on error
   */
  def deleteByPersonId(id: Long): Option[Int] = {
    implicit val loc = VL("PersonRole.deleteByPersonId")

    vld {
      DB.withConnection("s2") {
        implicit connection =>
          SQL("delete from person_role where person_id = {id}").
            on('id -> id).executeUpdate()
      }
    }.error.fold(e => None, s => Some(s) )
  }

  /**update the person_role table
   *
   * Remove any exiting person/role rows and add back the current one.
   * @param personId  person Id to remove then add
   * @param roleName    role name for the person
   * @param createdBy id of the person creating this record. Defaults to zero
   * @return None if either the delete or insert fails, otherwise the ID of the inserted person_role
   */
  def update(personId: Long, roleName: String, createdBy: Long = 0) = {

    Role.findByName(roleName) match {
      case Some(roleId) => {
        deleteByPersonId(personId)
        insert(personId, roleId, createdBy)
        }
      case _ => None
    }
  }
}


//case class Role(id: Long, name: String, createdAt: DateTime, updatedAt: DateTime, createdBy: Long, updatedBy: Long) {
//}
object Role {

  // Current roles
  val npAdmin  = "npAdmin"
  val npUser   = "npUser"
  val oemAdmin = "oemAdmin"
  val oemUser  = "oemUser"

  // insert into role values (0, 'npAdmin', 1, now(), 0, 0, null);
  // insert into role values (0, 'npUser',  1, now(), 0, 0, null);
  // insert into role values (0, 'oemAdmin',2, now(), 0, 0, null);
  // insert into role values (0, 'oemUser', 2, now(), 0, 0, null);
  //
  // insert into target values (0, 'roleSetup', now(), 0, 0, null);
  // insert into target values (0, 'userAccount', now(), 0, 0, null);
  //

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
   * @return  optional role ID on success
   */
  def findByName(roleName: String): Option[Long] = {
    implicit val loc = VL("Role.findByName")

    vld {
      DB.withConnection("s2") {
        implicit connection =>
          SQL("select id from role where name = {name}").on('name -> roleName).as(scalar[Long].singleOpt)
      }
    }.info.fold(e => None, s => s)
  }

}