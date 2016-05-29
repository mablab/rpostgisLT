-- create schema to store pgtraj
CREATE SCHEMA IF NOT EXISTS pgtraj;

/*
 * Create tables for the pgtraj schema
 */

CREATE TABLE IF NOT EXISTS pgtraj.animals (
    id          serial      PRIMARY KEY, 
    "name"      text        UNIQUE
);

CREATE TABLE IF NOT EXISTS pgtraj.trajectories (
    id          serial      PRIMARY KEY,
    "name"      text        UNIQUE,
    animal_id   int4        NOT NULL REFERENCES pgtraj.animals (id)
                            ON DELETE CASCADE
                            ON UPDATE CASCADE
);

CREATE TABLE IF NOT EXISTS pgtraj.bursts (
    id          serial      PRIMARY KEY,
    "name"      text        UNIQUE
);

CREATE TABLE IF NOT EXISTS pgtraj.steps (
    id          bigserial   PRIMARY KEY,
    step        geography   NOT NULL,
    "time"      timestamptz,
    dtime       INTERVAL,
    "length"    float8
);

CREATE TABLE IF NOT EXISTS pgtraj.t_rel_b_rel_s (
    id          serial      PRIMARY KEY,
    traj_id     int4        NOT NULL REFERENCES pgtraj.trajectories (id)
                            ON DELETE CASCADE
                            ON UPDATE CASCADE,
    burst_id    int4        REFERENCES pgtraj.bursts(id)
                            ON DELETE CASCADE
                            ON UPDATE CASCADE,
    step_id     int8        NOT NULL REFERENCES pgtraj.steps (id)
                            ON DELETE CASCADE
                            ON UPDATE CASCADE
);
-- create complex constraint
CREATE UNIQUE INDEX ts_b_null_idx 
ON pgtraj.t_rel_b_rel_s (traj_id, step_id)
WHERE burst_id IS NULL;


/*
 * Example queries
 */
-- query all bursts of animal named 'buksi'
SELECT pgtraj.bursts."name"
FROM 
    pgtraj.steps_rel_traj r,
    pgtraj.bursts b,
    pgtraj.trajectories t,
    pgtraj.animals a
WHERE
    a."name" = 'buksi' AND
    t.animal_id = a.id AND
    t.id = r.trajectory_id AND
    t.burst_id = b.id
;