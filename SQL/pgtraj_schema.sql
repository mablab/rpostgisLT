-- create schema to store pgtraj
/*
 * pgtraj_v4
 */

CREATE SCHEMA IF NOT EXISTS pgtraj;

CREATE TABLE pgtraj.pgtrajs (
    p_id        serial      PRIMARY KEY,
    p_name      text        NOT NULL UNIQUE 
);

CREATE TABLE pgtraj.animals (
    a_id        serial      PRIMARY KEY,
    a_name      text        NOT NULL UNIQUE
);

CREATE TABLE pgtraj.bursts (
    b_id        serial      PRIMARY KEY,
    b_name      text        UNIQUE,
    a_id        integer     NOT NULL REFERENCES pgtraj.animals (a_id)
);

/* Select corresponding animal name as default burst name (based on a_id)
 */
CREATE OR REPLACE FUNCTION pgtraj.b_name_default()
RETURNS trigger AS '
    BEGIN
        SELECT INTO NEW.b_name t.a_name
        FROM pgtraj.animals As t
        WHERE t.a_id = NEW.a_id;
        RETURN NEW;
    END;
' LANGUAGE plpgsql;

CREATE TRIGGER b_name_default
BEFORE INSERT OR UPDATE ON pgtraj.bursts
FOR EACH ROW
WHEN (NEW.b_name IS NULL AND NEW.a_id IS NOT NULL)
EXECUTE PROCEDURE pgtraj.b_name_default();


-- prevent table drop unless empty
-- it is handled on database administration level, by not allowing the user to drop tables
CREATE TABLE pgtraj.p_b_rel (
    p_id        integer     NOT NULL REFERENCES pgtraj.pgtrajs (p_id)
                            ON DELETE CASCADE,
    b_id        integer     NOT NULL REFERENCES pgtraj.bursts (b_id)
                            ON DELETE CASCADE,
    PRIMARY KEY (p_id, b_id)
);

CREATE TABLE pgtraj.steps (
    s_id        serial   PRIMARY KEY,
    step        geography   NOT NULL,
    "date"      timestamptz DEFAULT NULL,
    dt          interval    DEFAULT NULL
);

CREATE TABLE pgtraj.infolocs (
    i_id       serial      PRIMARY KEY,
    infoloc    json        DEFAULT NULL
);

CREATE TABLE pgtraj.s_i_b_rel (
    s_id        integer     NOT NULL REFERENCES pgtraj.steps (s_id)
                            ON DELETE CASCADE,
    i_id        integer     NOT NULL REFERENCES pgtraj.infolocs (i_id)
                            ON DELETE CASCADE,
    b_id        integer     NOT NULL REFERENCES pgtraj.bursts (b_id)
                            ON DELETE CASCADE,
    PRIMARY KEY (s_id, b_id)
);

-- create index on step geometry
CREATE INDEX step_idx ON pgtraj.steps USING gist (step);

-- add comments to the schema
COMMENT ON SCHEMA pgtraj IS 'Implements the pgtraj data model, based on the ltraj object class in R';

COMMENT ON TABLE pgtraj.pgtrajs IS 'Groups of trajectories, with unique names. Groups can be defined on any criteria, e.g. steps belonging to one ltraj object can form a group.';
COMMENT ON COLUMN pgtraj.pgtrajs.p_id IS 'Auto-generated numeric ID of pgtraj.';
COMMENT ON COLUMN pgtraj.pgtrajs.p_name IS 'Name or identifier of trajectory group, not null, unique.';

COMMENT ON TABLE pgtraj.bursts IS 'Contains burst information and their relation to animals.';
COMMENT ON COLUMN pgtraj.bursts.b_id IS 'Auto-generated numeric ID of burst.';
COMMENT ON COLUMN pgtraj.bursts.b_name IS 'Name or identifier of burst. Unique, defaults to the animal name based on the provided a_id.';
COMMENT ON COLUMN pgtraj.bursts.a_id IS 'Reference to animals.a_id. Not null.';

COMMENT ON TABLE pgtraj.animals IS 'Contains animal descriptions.';
COMMENT ON COLUMN pgtraj.animals.a_id IS 'Auto-generated numeric ID of animal.';
COMMENT ON COLUMN pgtraj.animals.a_name IS 'Name of the animal. Not null, unique.';

COMMENT ON TABLE pgtraj.steps IS 'Steps derived from relocations.';
COMMENT ON COLUMN pgtraj.steps.s_id IS 'Auto-generated numeric ID of step. Equal to the ID of the first of the two successive relocations that form the step.';
COMMENT ON COLUMN pgtraj.steps.step IS 'Geometry of the step.';
COMMENT ON COLUMN pgtraj.steps.date IS 'Timestamp of the first of the two successive locations that form the step.';
COMMENT ON COLUMN pgtraj.steps.dt IS 'Duration of the step.';

COMMENT ON TABLE pgtraj.infolocs IS 'Contains additional information on steps. Mirrors the infoloc ltraj attribute.';
COMMENT ON COLUMN pgtraj.infolocs.i_id IS 'Auto-generated numeric ID of infoloc.';
COMMENT ON COLUMN pgtraj.infolocs.infoloc IS 'Contains the additional information encoded in JSON.';

COMMENT ON TABLE pgtraj.p_b_rel IS 'Relates pgtraj and burst.';
COMMENT ON TABLE pgtraj.s_i_b_rel IS 'Relates step, infoloc and burst.';
