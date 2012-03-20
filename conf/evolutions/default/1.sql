# Evolution template. Put first changes here in straight SQL
# Ups and Downs comment lines must be there. Number your evolutions sequentially.  1.sql ... n.sql
# to re-rerun them you can delete the line referring to that evo in the play_evolutions
# table in the DB in question.

# --- !Ups

alter table exerciser add column vt_status tinyint not null default 0;

# --- !Downs

alter table exerciser drop column vt_status;
