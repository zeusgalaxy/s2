# Evolution template. Put first changes here in straight SQL
# Ups and Downs comment lines must be there. Number your evolutions sequentially.  1.sql ... n.sql
# to re-rerun them you can delete the line referring to that evo in the play_evolutions
# table in the DB in question.

# --- !Ups

CREATE TABLE club_exerciser_channel(
    club_id          INTEGER    NOT NULL,
    exerciser_id     INTEGER    NOT NULL,
    tv_channel_id    INTEGER    NOT NULL,
    created_at       DATETIME       NOT NULL,
    updated_at       DATETIME,
    created_by       INTEGER    NOT NULL,
    updated_by       INTEGER,
    PRIMARY KEY (club_id, exerciser_id, tv_channel_id),
    FOREIGN KEY (club_id)
    REFERENCES company(id),
    FOREIGN KEY (exerciser_id)
    REFERENCES exerciser_profile(id)
);

# --- !Downs

DROP TABLE IF EXISTS club_exerciser_channel;
