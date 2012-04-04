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



