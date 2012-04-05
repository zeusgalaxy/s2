insert into person values(null, "s2", "netpulse", "s2@netpulse.com", "19e359d99f3c4e3bdf8592078de921bc", "s2@netpulse.com", "5103369779", now(), 1, now(), now(), 0, 0, null, null);
insert into target values (null, "reportWorkoutLocations", 0, 0, 0, null);
insert into role values (null, "netpulse-admin", 0, 0, 0, null);
insert into person_role values(null, 0, 0, 0, null, 8234, 1);
insert into rights values(null, 1, 1, 1, 1, 0, 0, 0, 0, 0, 1, 1);


insert into person values(null, "s2a", "netpulse", "s2a@netpulse.com", "19e359d99f3c4e3bdf8592078de921bc", "s2a@netpulse.com", "5103369780", now(), 1, now(), now(), 0, 0, null, null);
insert into target values (null, "test", 0, 0, 0, null);
insert into role values (null, "netpulse-test", 0, 0, 0, null);
insert into person_role values(null, 0, 0, 0, null, 8235, 2);

// Last two are role and target. Goal is to allow s2 to access reportWorkoutLocations (but
// not test), and for s2a to access test but not reportWorkoutLocations:
insert into rights values(null, 1, 1, 1, 1, 0, 0, 0, 0, 0, 2, 2);


mysql> desc exerciser_profile;
+-----------------+---------------+------+-----+---------------------+-------+
| Field           | Type          | Null | Key | Default             | Extra |
+-----------------+---------------+------+-----+---------------------+-------+
| person_id       | int(11)       | NO   | PRI | NULL                |       |
| client_login    | varchar(40)   | YES  |     | NULL                |       |
| client_password | varchar(40)   | NO   |     | NULL                |       |
| weight          | decimal(6,2)  | NO   |     | 0.00                |       |
| gender          | char(1)       | NO   |     |                     |       |
| date_of_birth   | date          | NO   |     | 0000-00-00          |       |
| pic             | tinyint(4)    | NO   |     | NULL                |       |
| vt_token        | varchar(1000) | YES  |     |                     |       |
| vt_token_secret | varchar(1000) | YES  |     |                     |       |
| vt_user_id      | varchar(1000) | NO   |     | NULL                |       |
| vt_status       | tinyint(4)    | NO   |     | 0                   |       |
| home_club_id    | int(11)       | NO   |     | NULL                |       |
| created_at      | datetime      | NO   |     | NULL                |       |
| updated_at      | datetime      | NO   |     | 0000-00-00 00:00:00 |       |
| created_by      | int(11)       | NO   |     | NULL                |       |
| updated_by      | int(11)       | YES  |     | NULL                |       |
+-----------------+---------------+------+-----+---------------------+-------+

insert into exerciser_profile values(8233, "s2", "55", 180, "M", 0, 55, "", "", "", 0, 99, 0, 0, 0, null);
insert into exerciser_profile values(8236, "s2a", "55", 180, "M", 0, 55, "", "", "", 0, 99, 0, 0, 0, null);

