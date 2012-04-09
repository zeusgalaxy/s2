delete from rights where role_id = (select id from role where name = "netpulse-test-a");
delete from rights where role_id = (select id from role where name = "netpulse-test-b");
delete from role where name = "netpulse-test-a";
delete from role where name = "netpulse-test-b";
delete from target where name = "test-a";
delete from target where name = "test-b";
delete from person where portal_login = "s2-a@netpulse.com";
delete from person where portal_login = "s2-b@netpulse.com";


insert into role set id = null, name = "netpulse-test-a", role_group = 0, created_at = now(),
updated_at = 0, created_by = 0, updated_by = null;

insert into role set id = null, name = "netpulse-test-b", role_group = 0, created_at = now(),
updated_at = 0, created_by = 0, updated_by = null;


insert into target set id = null, name = "test-a", created_at = now(), updated_at = 0, created_by = 0, updated_by = null;

insert into target set id = null, name = "test-b", created_at = now(), updated_at = 0, created_by = 0, updated_by = null;


insert into person set id = null, company_id = null, role_id = (select id from role where name = "netpulse-test-a"),
first_name = "s2-a", last_name = "netpulse", portal_login = "s2-a@netpulse.com",
portal_password = "19e359d99f3c4e3bdf8592078de921bc", email = "s2-a@netpulse.com",
phone = "5103369779", last_login_dt = 0, active_status = 1, status_dt = 0, created_at = now(),
updated_at = 0, created_by = 0, updated_by = null, address_id = null;

insert into person set id = null, company_id = null, role_id = (select id from role where name = "netpulse-test-b"),
first_name = "s2-b", last_name = "netpulse", portal_login = "s2-b@netpulse.com",
portal_password = "19e359d99f3c4e3bdf8592078de921bc", email = "s2-b@netpulse.com",
phone = "5103369780", last_login_dt = 0, active_status = 1, status_dt = 0, created_at = now(),
updated_at = 0, created_by = 0, updated_by = null, address_id = null;


insert into rights set id = null, c = 1, r = 1, u = 1, d = 1, filter = 0, created_at = now(), updated_at = 0,
created_by = 0, updated_by = null, role_id = (select id from role where name = "netpulse-test-a"),
target_id = (select id from target where name = "test-a");

insert into rights set id = null, c = 1, r = 1, u = 1, d = 1, filter = 0, created_at = now(), updated_at = 0,
created_by = 0, updated_by = null, role_id = (select id from role where name = "netpulse-test-b"),
target_id = (select id from target where name = "test-b");


insert into exerciser_profile set person_id = (select id from person where portal_login = "s2-a@netpulse.com"),
client_login = "s2-a", client_password = "55", weight = 180,
gender = "M", date_of_birth = "1970-03-01", pic = 55, vt_token = null, vt_token_secret = null,
vt_user_id = "", vt_status = 0, home_club_id = 99, created_at = now(), updated_at = 0, created_by = 0, updated_by = null;

insert into exerciser_profile set person_id = (select id from person where portal_login = "s2-b@netpulse.com"),
client_login = "s2-b", client_password = "55", weight = 180,
gender = "M", date_of_birth = "1970-03-01", pic = 55, vt_token = null, vt_token_secret = null,
vt_user_id = "", vt_status = 0, home_club_id = 99, created_at = now(), updated_at = 0, created_by = 0, updated_by = null;

