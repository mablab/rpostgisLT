-- create schema to store pgtraj
/*
 * pgtraj_v6
 */

-- create the schema and set the default search path
-- CREATE SCHEMA IF NOT EXISTS pgtraj;
-- SHOW search_path;
-- SET search_path TO pgtraj,public;

CREATE TABLE pgtrajs (
    p_id        serial      PRIMARY KEY,
    p_name      text        NOT NULL UNIQUE,
    r_proj      text,
    comment     text
);

CREATE TABLE animals (
    a_id        serial      PRIMARY KEY,
    a_name      text        NOT NULL UNIQUE
);

CREATE TABLE bursts (
    b_id        serial      PRIMARY KEY,
    b_name      text        UNIQUE,
    a_id        integer     NOT NULL REFERENCES animals (a_id)
);

/* Select corresponding animal name as default burst name (based on a_id)
 */
CREATE OR REPLACE FUNCTION b_name_default()
RETURNS trigger AS '
    BEGIN
        SELECT INTO NEW.b_name t.a_name
        FROM animals As t
        WHERE t.a_id = NEW.a_id;
        RETURN NEW;
    END;
' LANGUAGE plpgsql;

CREATE TRIGGER b_name_default
BEFORE INSERT OR UPDATE ON bursts
FOR EACH ROW
WHEN (NEW.b_name IS NULL AND NEW.a_id IS NOT NULL)
EXECUTE PROCEDURE b_name_default();


-- prevent table drop unless empty
-- it is handled on database administration level, by not allowing the user to drop tables
CREATE TABLE p_b_rel (
    p_id        integer     NOT NULL REFERENCES pgtrajs (p_id)
                            ON DELETE CASCADE,
    b_id        integer     NOT NULL REFERENCES bursts (b_id)
                            ON DELETE CASCADE,
    PRIMARY KEY (p_id, b_id)
);

CREATE TABLE steps (
    s_id        serial      PRIMARY KEY,
    r_rowname   text        DEFAULT NULL,
    reloc1      geometry    DEFAULT NULL,
    step        geometry    DEFAULT NULL,
    "date"      timestamptz DEFAULT NULL,
    dt          interval    DEFAULT NULL
);

CREATE TABLE infolocs (
    i_id       serial      PRIMARY KEY,
    infoloc    json        DEFAULT NULL
);

CREATE TABLE s_i_b_rel (
    s_id        integer     NOT NULL REFERENCES steps (s_id)
                            ON DELETE CASCADE,
    i_id        integer     REFERENCES infolocs (i_id)
                            ON DELETE CASCADE,
    b_id        integer     NOT NULL REFERENCES bursts (b_id)
                            ON DELETE CASCADE,
    PRIMARY KEY (s_id, b_id)
);

-- create index on step geometry
CREATE INDEX step_idx ON steps USING gist (step);

-- add comments to the schema

COMMENT ON TABLE pgtrajs IS 'Groups of trajectories, with unique names. Groups can be defined on any criteria, e.g. steps belonging to one ltraj object can form a group.';
COMMENT ON COLUMN pgtrajs.p_id IS 'Auto-generated numeric ID of ';
COMMENT ON COLUMN pgtrajs.p_name IS 'Name or identifier of trajectory group, not null, unique.';
COMMENT ON COLUMN pgtrajs.r_proj IS 'Projection string of the ltraj, imported from R.';

COMMENT ON TABLE bursts IS 'Contains burst information and their relation to animals.';
COMMENT ON COLUMN bursts.b_id IS 'Auto-generated numeric ID of burst.';
COMMENT ON COLUMN bursts.b_name IS 'Name or identifier of burst. Unique, defaults to the animal name based on the provided a_id.';
COMMENT ON COLUMN bursts.a_id IS 'Reference to animals.a_id. Not null.';

COMMENT ON TABLE animals IS 'Contains animal descriptions.';
COMMENT ON COLUMN animals.a_id IS 'Auto-generated numeric ID of animal.';
COMMENT ON COLUMN animals.a_name IS 'Name of the animal. Not null, unique.';

COMMENT ON TABLE steps IS 'Steps derived from relocations.';
COMMENT ON COLUMN steps.s_id IS 'Auto-generated numeric ID of step. Equal to the ID of the first of the two successive relocations that form the step.';
COMMENT ON COLUMN steps.r_rowname IS 'Row name in the ltraj. This value is used for backward referencing between pgtraj and ltraj.';
COMMENT ON COLUMN steps.step IS 'Geometry of the step.';
COMMENT ON COLUMN steps.date IS 'Timestamp of the first of the two successive locations that form the step.';
COMMENT ON COLUMN steps.dt IS 'Duration of the step.';

COMMENT ON TABLE infolocs IS 'Contains additional information on steps. Mirrors the infoloc ltraj attribute.';
COMMENT ON COLUMN infolocs.i_id IS 'Auto-generated numeric ID of infoloc.';
COMMENT ON COLUMN infolocs.infoloc IS 'Contains the additional information encoded in JSON.';

COMMENT ON TABLE p_b_rel IS 'Relates pgtraj and burst.';
COMMENT ON TABLE s_i_b_rel IS 'Relates step, infoloc and burst.';

-- restore search path to public first
-- SET search_path TO "$user",public;
