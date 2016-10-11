CREATE TABLE pgtraj (
    id                 serial      PRIMARY KEY,
    pgtraj_name        text        UNIQUE NOT NULL,
    proj4string        text        DEFAULT NULL,
    time_zone          text        DEFAULT NULL,
    note               text        DEFAULT NULL,
	insert_timestamp	timestamptz DEFAULT now()
);


CREATE TABLE animal_burst (
    id               serial      PRIMARY KEY,
    burst_name       text        NOT NULL ,
    animal_name      text        NOT NULL,
    pgtraj_id        integer     NOT NULL REFERENCES pgtraj (id)
                                 ON DELETE CASCADE,
	info_cols		   text[][][]  DEFAULT NULL,
    CONSTRAINT burst_pgtraj_unique UNIQUE (burst_name, pgtraj_id)
);


CREATE TABLE relocation (
    id               serial      PRIMARY KEY,
    geom             geometry    DEFAULT NULL,
    relocation_time  timestamptz DEFAULT NULL,
    orig_id	     integer 	 DEFAULT NULL --added for infolocs support
);

CREATE TABLE step (
    id                serial      PRIMARY KEY,
    relocation_id_1   integer     DEFAULT NULL REFERENCES relocation (id)
                                  ON DELETE CASCADE,
    relocation_id_2   integer     DEFAULT NULL REFERENCES relocation (id)
                                  ON DELETE SET NULL,
    dt                interval    DEFAULT NULL,
    r_rowname         text        DEFAULT NULL,
    r2n               float8      DEFAULT NULL,
    rel_angle         float8      DEFAULT NULL
);
/*
CREATE TABLE infoloc (
	id                serial      PRIMARY KEY,
    infoloc           json        DEFAULT NULL
);
*/
CREATE TABLE s_b_rel (
    step_id          integer     NOT NULL REFERENCES step (id)
                                 ON DELETE CASCADE,
    --infoloc_id       integer     REFERENCES infoloc (id)
    --                             ON DELETE SET NULL,
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
COMMENT ON COLUMN pgtraj.insert_timestamp IS 'Time when pgtraj was created.';

COMMENT ON TABLE animal_burst IS 'Contains animal and burst information and their relation to pgtrajs.';
COMMENT ON COLUMN animal_burst.id IS 'Auto-generated numeric ID.';
COMMENT ON COLUMN animal_burst.burst_name IS 'Name of burst.';
COMMENT ON COLUMN animal_burst.animal_name IS 'Name of animal.';
COMMENT ON COLUMN animal_burst.pgtraj_id IS 'Foreign key to pgtraj records.';
COMMENT ON COLUMN animal_burst.info_cols IS 'Array holding R data type definitions for infolocs columns. Do not edit.';


COMMENT ON TABLE step IS 'Steps derived from relocations.';
COMMENT ON COLUMN step.id IS 'Auto-generated numeric ID.';
COMMENT ON COLUMN step.relocation_id_1 IS 'The first of the two successive relocations that form a step.';
COMMENT ON COLUMN step.relocation_id_2 IS 'The second of the two successive relocations that form a step.';
COMMENT ON COLUMN step.dt IS 'Duration of the step.';
COMMENT ON COLUMN step.r_rowname IS 'Row name in the ltraj. This value is used for backward referencing between pgtraj and ltraj.';
COMMENT ON COLUMN step.r2n IS 'R2n parameter copied from the ltraj on import from R.';
COMMENT ON COLUMN step.rel_angle IS 'Rel.angle parameter copied from the ltraj on import from R.';
COMMENT ON TABLE relocation IS 'Relocation geometry and time stamp.';
COMMENT ON COLUMN relocation.id IS 'Auto-generated numeric ID.';
COMMENT ON COLUMN relocation.geom IS 'Geometry of the relocation.';
COMMENT ON COLUMN relocation.relocation_time IS 'Time stamp of the relocation.';
COMMENT ON COLUMN relocation.orig_id IS 'ID number from the original relocations table in the database.';

--COMMENT ON TABLE infoloc IS 'Contains additional information on steps. Mirrors the infoloc ltraj attribute.';
--COMMENT ON COLUMN infoloc.id IS 'Auto-generated numeric ID of infoloc.';
--COMMENT ON COLUMN infoloc.infoloc IS 'Contains the additional information encoded in JSON.';

COMMENT ON TABLE s_b_rel IS 'Relates step and burst.';

-- insert_pgtraj fn
CREATE OR REPLACE FUNCTION insert_pgtraj(INT)
  RETURNS boolean AS
$BODY$
DECLARE pg record;
BEGIN
-- insert pgtrajs
INSERT INTO pgtraj (pgtraj_name, proj4string, time_zone, note)
		SELECT DISTINCT pgtraj_name, proj4string, time_zone, note
		FROM zgaqtsn_temp
		ORDER BY pgtraj_name;

IF $1 = 2 THEN
-- loop bursts
	FOR pg IN 
	SELECT DISTINCT pgtraj_name, animal_name, burst_name FROM zgaqtsn_temp ORDER BY pgtraj_name, burst_name
	LOOP
		ALTER TABLE relocation ADD COLUMN mark integer;
		ALTER TABLE step ADD COLUMN mark integer;
		--burst
		INSERT INTO animal_burst (burst_name, animal_name, pgtraj_id)
			SELECT pg.burst_name, pg.animal_name, pgtraj.id
			FROM pgtraj
			WHERE pg.pgtraj_name = pgtraj.pgtraj_name;
		-- relocations
		INSERT INTO relocation (geom, relocation_time, orig_id, mark) --added orig_id for infoloc support
			SELECT geom, relocation_time ,id, 1	--added orig_id for infoloc support
			FROM zgaqtsn_temp
			WHERE pgtraj_name = pg.pgtraj_name
			AND animal_name = pg.animal_name
			AND burst_name = pg.burst_name
			ORDER BY relocation_time;
		-- steps	
		WITH relos AS (
			SELECT
				a.id AS relocation_id_1,
				b.id AS relocation_id_2,
				b.relocation_time - a.relocation_time AS dt,
				a.mark
			FROM (SELECT * FROM relocation WHERE mark = 1) a 
			LEFT OUTER JOIN LATERAL 
				(SELECT c.id, c.relocation_time
				   FROM relocation c
				   WHERE mark = 1
				   AND a.relocation_time < c.relocation_time
				   ORDER BY c.relocation_time ASC
				   LIMIT 1
				 ) AS b 
			ON TRUE
			)
		INSERT INTO step (relocation_id_1, relocation_id_2, dt, mark)
			SELECT relocation_id_1, relocation_id_2, dt, 1
			FROM relos;
		-- step-burst rel
		INSERT INTO s_b_rel (step_id, animal_burst_id)
			SELECT a.id, a.burst_id
			FROM (SELECT step.id as id, pg.burst_name, pgtraj.id as pg_id, animal_burst.id as burst_id 
				 FROM step, animal_burst, pgtraj
				 WHERE step.mark = 1
				 AND pgtraj.id = animal_burst.pgtraj_id
				 AND pg.pgtraj_name = pgtraj.pgtraj_name
				 AND pg.burst_name = animal_burst.burst_name) a;
		-- drop mark columns
		ALTER TABLE relocation DROP COLUMN mark;
		ALTER TABLE step DROP COLUMN mark;
	END LOOP;
ELSE
-- loop bursts for type 1 (no time)
	FOR pg IN 
	SELECT DISTINCT pgtraj_name, animal_name, burst_name FROM zgaqtsn_temp ORDER BY pgtraj_name, burst_name
	LOOP
		ALTER TABLE relocation ADD COLUMN mark integer;
		ALTER TABLE step ADD COLUMN mark integer;
		--burst
		INSERT INTO animal_burst (burst_name, animal_name, pgtraj_id)
			SELECT pg.burst_name, pg.animal_name, pgtraj.id
			FROM pgtraj
			WHERE pg.pgtraj_name = pgtraj.pgtraj_name;
		-- relocations
		INSERT INTO relocation (geom, orig_id, mark) --added orig_id for infoloc support
			SELECT geom ,id, 1	--added orig_id for infoloc support
			FROM zgaqtsn_temp
			WHERE pgtraj_name = pg.pgtraj_name
			AND animal_name = pg.animal_name
			AND burst_name = pg.burst_name
			ORDER BY id;
		-- steps	
		WITH relos AS (
			SELECT
				a.id AS relocation_id_1,
				b.id AS relocation_id_2,
				a.mark
			FROM (SELECT * FROM relocation WHERE mark = 1) a 
			LEFT OUTER JOIN LATERAL 
				(SELECT c.id
				   FROM relocation c
				   WHERE mark = 1
				   AND a.id < c.id
				   ORDER BY c.id ASC
				   LIMIT 1
				 ) AS b 
			ON TRUE
			)
		INSERT INTO step (relocation_id_1, relocation_id_2, mark)
			SELECT relocation_id_1, relocation_id_2, 1
			FROM relos;
		-- step-burst rel
		INSERT INTO s_b_rel (step_id, animal_burst_id)
			SELECT a.id, a.burst_id
			FROM (SELECT step.id as id, pg.burst_name, pgtraj.id as pg_id, animal_burst.id as burst_id 
				 FROM step, animal_burst, pgtraj
				 WHERE step.mark = 1
				 AND pgtraj.id = animal_burst.pgtraj_id
				 AND pg.pgtraj_name = pgtraj.pgtraj_name
				 AND pg.burst_name = animal_burst.burst_name) a;
		-- drop mark columns
		ALTER TABLE relocation DROP COLUMN mark;
		ALTER TABLE step DROP COLUMN mark;
	END LOOP;
END IF;
RETURN TRUE;
END;
$BODY$
  LANGUAGE plpgsql VOLATILE
  COST 100;


