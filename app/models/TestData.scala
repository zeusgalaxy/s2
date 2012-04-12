package models

import utils._
import play.api.db._
import play.api.Play.current

import org.joda.time._
import anorm._
import anorm.SqlParser._
import play.api.Logger
import scalaz.{Node => _, _}

object TestData {

  val testSqlStmts =
    """
    
delete from rights where role_id = (select id from role where name = 'netpulse-test-a');
delete from rights where role_id = (select id from role where name = 'netpulse-test-b');
delete from exerciser_profile where client_login = 's2-a';
delete from exerciser_profile where client_login = 's2-b';
delete from person where portal_login = 's2-a@netpulse.com';
delete from person where portal_login = 's2-b@netpulse.com';
delete from role where name = 'netpulse-test-a';
delete from role where name = 'netpulse-test-b';
delete from target where name = 'test-a';
delete from target where name = 'test-b';


insert into role set id = null, name = 'netpulse-test-a', role_group = 0, created_at = now(),
updated_at = 0, created_by = 0, updated_by = null;

insert into role set id = null, name = 'netpulse-test-b', role_group = 0, created_at = now(),
updated_at = 0, created_by = 0, updated_by = null;


insert into target set id = null, name = 'test-a', created_at = now(), updated_at = 0, created_by = 0, updated_by = null;

insert into target set id = null, name = 'test-b', created_at = now(), updated_at = 0, created_by = 0, updated_by = null;


insert into person set id = null, company_id = 2, role_id = (select id from role where name = 'netpulse-test-a'),
first_name = 's2-a', last_name = 'netpulse', portal_login = 's2-a@netpulse.com',
portal_password = '19e359d99f3c4e3bdf8592078de921bc', email = 's2-a@netpulse.com',
phone = '5103369779', last_login_dt = 0, active_status = 1, status_dt = 0, created_at = now(),
updated_at = 0, created_by = 0, updated_by = null, address_id = null;

insert into person set id = null, company_id = 2, role_id = (select id from role where name = 'netpulse-test-b'),
first_name = 's2-b', last_name = 'netpulse', portal_login = 's2-b@netpulse.com',
portal_password = '19e359d99f3c4e3bdf8592078de921bc', email = 's2-b@netpulse.com',
phone = '5103369780', last_login_dt = 0, active_status = 1, status_dt = 0, created_at = now(),
updated_at = 0, created_by = 0, updated_by = null, address_id = null;


insert into rights set id = null, c = 1, r = 1, u = 1, d = 1, filter = 0, created_at = now(), updated_at = 0,
created_by = 0, updated_by = null, role_id = (select id from role where name = 'netpulse-test-a'),
target_id = (select id from target where name = 'test-a');

insert into rights set id = null, c = 1, r = 1, u = 1, d = 1, filter = 0, created_at = now(), updated_at = 0,
created_by = 0, updated_by = null, role_id = (select id from role where name = 'netpulse-test-b'),
target_id = (select id from target where name = 'test-b');


insert into exerciser_profile set person_id = (select id from person where portal_login = 's2-a@netpulse.com'),
client_login = 's2-a', client_password = '55', weight = 180,
gender = 'M', date_of_birth = '1970-03-01', pic = 55, vt_token = null, vt_token_secret = null,
vt_user_id = '', vt_status = 0, home_club_id = 99, created_at = now(), updated_at = 0, created_by = 0, updated_by = null;

insert into exerciser_profile set person_id = (select id from person where portal_login = 's2-b@netpulse.com'),
client_login = 's2-b', client_password = '55', weight = 180,
gender = 'M', date_of_birth = '1970-03-01', pic = 55, vt_token = null, vt_token_secret = null,
vt_user_id = '', vt_status = 0, home_club_id = 99, created_at = now(), updated_at = 0, created_by = 0, updated_by = null;


    """

  // Seed Data
  val seedSqlStmts = """
    insert into company set id=2, name='Netpulse', phone ='', fax ='', email='support@netpulse.com', url='www.netpulse.com', created_at=now(),
    created_by=8234, company_type_id=1, address_id=56390;

    insert into company set id=81, name='LifeFitness', phone='', fax='', email='support@lifefitness.com', url='www.lifefitness.com', created_at=now(),
    created_by=8234, company_type_id=2, address_id=56390;

    # NP Admin and Lifefitness Roles
    insert into role set id=1, name='npAdmin',  role_group=1, created_at=now(), created_by=8234;
    insert into role set id=2, name='npUser',   role_group=1, created_at=now(), created_by=8234;
    insert into role set id=3, name='oemAdmin', role_group=2, created_at=now(), created_by=8234;
    insert into role set id=4, name='oemUser',  role_group=1, created_at=now(), created_by=8234;
    insert into role set id=5, name='exerciser',role_group=100, created_at=now(), created_by=8234;

    # Basic targets
    insert into target set name='user',       created_at=now(), created_by=8234;
    insert into target set name='roleSetup',  created_at=now(), created_by=8234;
    insert into target set name='reportWorkoutLocation', created_at=now(), created_by=8234;

    # Rights for NP Admin
    insert into rights set c=1, r=1, u=1, d=1, filter=0, created_at=now(), created_by=8234, role_id=1 target_id=1;
    insert into rights set c=1, r=1, u=1, d=1, filter=0, created_at=now(), created_by=8234, role_id=2 target_id=2;
    insert into rights set c=1, r=1, u=1, d=1, filter=0, created_at=now(), created_by=8234, role_id=1 target_id=3;

    # Rights for Lifefitness Roles
    insert into rights (c, r, u, d, filter, created_at, created_by, role_id, target_id)
      select 1, 1, 1, 1, 1, now(), 8234, id, col2 from role join
      (select id as col2 from target where name ='user') u where name='oemAdmin';

    insert into rights (c, r, u, d, filter, created_at, created_by, role_id, target_id)
      select 1, 1, 1, 1, 1, now(), 8234, id, col2 from role join
      (select id as col2 from target where name ='roleSetup') u where name='oemAdmin';

  """



  def setup(sql: String): Boolean = {

    implicit val loc = VL("TestData.setup")

    vld {
      DB.withConnection("s2") {
        implicit connection => {
          sql.split(";").map(_.trim).filter(!_.isEmpty).foreach(SQL(_).execute)
        }
      }
    }.error.fold(e => {println(e.toString()); false}, s => true)
  }
}