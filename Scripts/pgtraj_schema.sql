-- create schema to store pgtraj
CREATE SCHEMA IF NOT EXISTS pgtraj;

/*
 * pgtraj_v2
 */

CREATE TABLE IF NOT EXISTS pgtraj.traj_group (
    g_id        serial      PRIMARY KEY,
    g_name      text        NOT NULL UNIQUE 
);

CREATE TABLE IF NOT EXISTS pgtraj.bursts (
    b_id        serial      PRIMARY KEY,
    b_name      text        NOT NULL UNIQUE,
    animal_name text        NOT NULL UNIQUE,
    g_id        int4        NOT NULL REFERENCES pgtraj.traj_group (g_id)
                            ON DELETE CASCADE
                            ON UPDATE CASCADE
);

CREATE TABLE IF NOT EXISTS pgtraj.steps (
    s_id        int8        PRIMARY KEY,
    step        geography   NOT NULL,
    "date"      timestamptz,
    dt          INTERVAL,
    b_id        int4        NOT NULL REFERENCES pgtraj.bursts (b_id)
                            ON UPDATE CASCADE
);
CREATE INDEX step_idx ON pgtraj.steps USING gist (step);

COMMENT ON SCHEMA pgtraj IS 'Implements the pgtraj data model';
COMMENT ON TABLE pgtraj.traj_group IS 'Groups of trajectories, with unique names. Groups can be defined on any criteria, for example a set of trajectories that are relevant for a particular project can form a group.';
COMMENT ON COLUMN pgtraj.traj_group.g_id IS 'Numeric ID of trajectory group for internal use.';
COMMENT ON COLUMN pgtraj.traj_group.g_name IS 'Name or identifier of trajectory group for external use, not null, unique.';
COMMENT ON TABLE pgtraj.bursts IS 'Burst and animal identifiers and their relation to trajectory groups. Both burst and animal names are unique across trajectory groups.';
COMMENT ON COLUMN pgtraj.bursts.b_id IS 'Numeric ID of burst for internal use.';
COMMENT ON COLUMN pgtraj.bursts.b_name IS 'Name or identifier of burst for external use, not null, unique.';
COMMENT ON COLUMN pgtraj.bursts.animal_name IS 'Name or identifier of animal for external use, not null, unique.';
COMMENT ON COLUMN pgtraj.bursts.g_id IS 'ID of trajectory group.';
COMMENT ON TABLE pgtraj.steps IS 'Steps derived from locations.';
COMMENT ON COLUMN pgtraj.steps.s_id IS 'Numeric ID of steps. Equal to the ID of the first of the two successive locations that form the step.';
COMMENT ON COLUMN pgtraj.steps.step IS 'Geometry of the step.';
COMMENT ON COLUMN pgtraj.steps.date IS 'Timestamp of the first of the two successive locations that form the step.';
COMMENT ON COLUMN pgtraj.steps.dt IS 'Duration of the step.';
COMMENT ON COLUMN pgtraj.steps.b_id IS 'ID of the burst to which the step belongs to, not null.';

