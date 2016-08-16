-- create schema to store pgtraj
/*
 * pgtraj_v7
 */

-- create the schema and set the default search path
--CREATE SCHEMA IF NOT EXISTS traj_schema;
--SHOW search_path;
--SET search_path TO traj_schema,public;

CREATE TABLE pgtraj (
    id                 serial      PRIMARY KEY,
    pgtraj_name        text        UNIQUE NOT NULL,
    proj4string        text        DEFAULT NULL,
    time_zone          text        DEFAULT NULL,
    note               text        DEFAULT NULL
);


CREATE TABLE animal_burst (
    id               serial      PRIMARY KEY,
    burst_name       text        NOT NULL ,
    animal_name      text        NOT NULL,
    pgtraj_id        integer     NOT NULL REFERENCES pgtraj (id)
                                 ON DELETE CASCADE,
    CONSTRAINT burst_pgtraj_unique UNIQUE (burst_name, pgtraj_id)
);


CREATE TABLE relocation (
    id               serial      PRIMARY KEY,
    geom             geometry    DEFAULT NULL,
    relocation_time  timestamptz DEFAULT NULL
);

CREATE TABLE step (
    id                serial      PRIMARY KEY,
    relocation_id_1   integer     DEFAULT NULL REFERENCES relocation (id)
                                  ON DELETE CASCADE,
    relocation_id_2   integer     DEFAULT NULL REFERENCES relocation (id)
                                  ON DELETE SET NULL,
    dt                interval    DEFAULT NULL,
    r_rowname         text        DEFAULT NULL
);

CREATE TABLE infoloc (
    id                serial      PRIMARY KEY,
    infoloc           json        DEFAULT NULL
);

CREATE TABLE s_i_b_rel (
    step_id          integer     NOT NULL REFERENCES step (id)
                                 ON DELETE CASCADE,
    infoloc_id       integer     REFERENCES infoloc (id)
                                 ON DELETE CASCADE,
    animal_burst_id  integer     NOT NULL REFERENCES animal_burst (id)
                                 ON DELETE CASCADE,
    PRIMARY KEY (step_id, animal_burst_id)
);

-- create index on step geometry
CREATE INDEX relocation_geom_idx ON relocation USING gist (geom);
CREATE INDEX relocation_time_idx ON relocation USING btree (relocation_time);

-- add comments to the schema

COMMENT ON TABLE pgtraj IS 'Groups of trajectories, with unique names. Groups can be defined on any criteria, e.g. steps belonging to one ltraj object can form a group.';
COMMENT ON COLUMN pgtraj.id IS 'Auto-generated numeric ID.';
COMMENT ON COLUMN pgtraj.pgtraj_name IS 'Name or identifier of trajectory group, not null.';
COMMENT ON COLUMN pgtraj.proj4string IS 'A PROJ.4 projection string of the ltraj, imported from R.';
COMMENT ON COLUMN pgtraj.time_zone IS 'Time zone of the imported trajectory.';
COMMENT ON COLUMN pgtraj.note IS 'User comment.';

COMMENT ON TABLE animal_burst IS 'Contains animal and burst information and their relation to pgtrajs.';
COMMENT ON COLUMN animal_burst.id IS 'Auto-generated numeric ID.';
COMMENT ON COLUMN animal_burst.burst_name IS 'Name of burst.';
COMMENT ON COLUMN animal_burst.animal_name IS 'Name of animal.';
COMMENT ON COLUMN animal_burst.pgtraj_id IS 'Foreign key to pgtraj records.';

COMMENT ON TABLE step IS 'Steps derived from relocations.';
COMMENT ON COLUMN step.id IS 'Auto-generated numeric ID.';
COMMENT ON COLUMN step.relocation_id_1 IS 'The first of the two successive relocations that form a step.';
COMMENT ON COLUMN step.relocation_id_2 IS 'The second of the two successive relocations that form a step.';
COMMENT ON COLUMN step.dt IS 'Duration of the step.';

COMMENT ON TABLE relocation IS 'Relocation geometry and time stamp.';
COMMENT ON COLUMN relocation.id IS 'Auto-generated numeric ID.';
COMMENT ON COLUMN relocation.geom IS 'Geometry of the relocation.';
COMMENT ON COLUMN relocation.relocation_time IS 'Time stamp of the relocation.';
COMMENT ON COLUMN relocation.r_rowname IS 'Row name in the ltraj. This value is used for backward referencing between pgtraj and ltraj.';

COMMENT ON TABLE infoloc IS 'Contains additional information on steps. Mirrors the infoloc ltraj attribute.';
COMMENT ON COLUMN infoloc.id IS 'Auto-generated numeric ID of infoloc.';
COMMENT ON COLUMN infoloc.infoloc IS 'Contains the additional information encoded in JSON.';

COMMENT ON TABLE s_i_b_rel IS 'Relates step, infoloc and burst.';

-- restore search path to public first
-- SET search_path TO "$user",public;

