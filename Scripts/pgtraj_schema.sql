-- create schema to store pgtraj
/*
 * pgtraj_v2
 */

CREATE SCHEMA IF NOT EXISTS pgtraj;

-- create ID sequences for using if the user does't provide IDs 
CREATE SEQUENCE pgtraj_id_seq;
CREATE SEQUENCE animal_id_seq;
CREATE SEQUENCE bursts_id_seq;
CREATE SEQUENCE steps_id_seq;

CREATE TABLE pgtraj.pgtraj (
    p_id        integer     PRIMARY KEY,
    p_name      text        NOT NULL UNIQUE 
);

CREATE TABLE pgtraj.animal (
    a_id        integer     PRIMARY KEY,    --are they automatically generated, or the user can provide his own?
    a_name      text        UNIQUE
);

CREATE TABLE pgtraj.bursts (
    b_id        integer     PRIMARY KEY,
    b_name      text        NOT NULL UNIQUE,
    a_id        integer     NOT NULL REFERENCES pgtraj.animal (a_id)
);

-- prevent table drop unless empty
-- it is handled on database administration level, by not allowing the user to drop tables
CREATE TABLE pgtraj.p_b_rel (
    p_id        integer     NOT NULL REFERENCES pgtraj.pgtraj (p_id)
                            ON DELETE CASCADE,
    b_id        integer     NOT NULL REFERENCES pgtraj.bursts (b_id)
                            ON DELETE CASCADE
);

CREATE TABLE pgtraj.steps (
    s_id        int8        PRIMARY KEY,
    step        geography   NOT NULL,
    "date"      timestamptz,
    dt          interval
);

CREATE TABLE pgtraj.s_b_rel (
    s_id        integer     NOT NULL REFERENCES pgtraj.steps (s_id)
                            ON DELETE CASCADE,
    b_id        integer     NOT NULL REFERENCES pgtraj.bursts (b_id)
                            ON DELETE CASCADE
);

-- create index
CREATE INDEX step_idx ON pgtraj.steps USING gist (step);

-- add comments
COMMENT ON SCHEMA pgtraj IS 'Implements the pgtraj data model';
COMMENT ON TABLE pgtraj.traj_group IS 'Groups of trajectories, with unique names. Groups can be defined on any criteria, for example a set of trajectories that are relevant for a particular project can form a group.';
COMMENT ON COLUMN pgtraj.traj_group.p_id IS 'Numeric ID of trajectory group for internal use.';
COMMENT ON COLUMN pgtraj.traj_group.p_name IS 'Name or identifier of trajectory group for external use, not null, unique.';
COMMENT ON TABLE pgtraj.bursts IS 'Burst and animal identifiers and their relation to trajectory groups. Both burst and animal names are unique across trajectory groups.';
COMMENT ON COLUMN pgtraj.bursts.b_id IS 'Numeric ID of burst for internal use.';
COMMENT ON COLUMN pgtraj.bursts.b_name IS 'Name or identifier of burst for external use, not null, unique.';
COMMENT ON COLUMN pgtraj.bursts.animal_name IS 'Name or identifier of animal for external use, not null, unique.';
COMMENT ON COLUMN pgtraj.bursts.p_id IS 'ID of trajectory group.';
COMMENT ON TABLE pgtraj.steps IS 'Steps derived from locations.';
COMMENT ON COLUMN pgtraj.steps.s_id IS 'Numeric ID of steps. Equal to the ID of the first of the two successive locations that form the step.';
COMMENT ON COLUMN pgtraj.steps.step IS 'Geometry of the step.';
COMMENT ON COLUMN pgtraj.steps.date IS 'Timestamp of the first of the two successive locations that form the step.';
COMMENT ON COLUMN pgtraj.steps.dt IS 'Duration of the step.';
COMMENT ON COLUMN pgtraj.steps.b_id IS 'ID of the burst to which the step belongs to, not null.';

