/*
 * Testing the pgtraj_v2
 */
INSERT INTO pgtraj.traj_group (g_name) VALUES ('group1');
INSERT INTO pgtraj.traj_group (g_name) VALUES ('group2');

INSERT INTO pgtraj.bursts (
    b_name,
    animal_name,
    g_id
) VALUES (
    'continental',
    'migrating animal',
    1
);
INSERT INTO pgtraj.bursts (
    b_name,
    animal_name,
    g_id
) VALUES (
    'large',
    'wood stork1',
    1
);
INSERT INTO pgtraj.bursts (
    b_name,
    animal_name,
    g_id
) VALUES (
    'large2',
    'wood stork2',
    1
);
INSERT INTO pgtraj.bursts (
    b_name,
    animal_name,
    g_id
) VALUES (
    'medium',
    'sea turtle',
    2
);
INSERT INTO pgtraj.bursts (
    b_name,
    animal_name,
    g_id
) VALUES (
    'small',
    'small animal',
    2
);

-- insert steps
INSERT INTO pgtraj.steps (
    s_id,
    step,
    date,
    dt,
    b_id
) (
    SELECT 
        e.startgid AS s_id,
        e.step_geog AS step,
        e.time AS date,
        e.dt,
        r.b_id
    FROM example_data.steps AS e INNER JOIN pgtraj.bursts AS r
    ON r.b_name = e.id
);


/* pgtraj testing v4
 */

INSERT INTO pgtraj.animals (a_id, a_name) 
SELECT nextval('pgtraj.animals_id_seq'), 'buksi';
INSERT INTO pgtraj.animals (a_id, a_name) 
SELECT nextval('pgtraj.animals_id_seq'), 'kutya';

INSERT INTO pgtraj.bursts (b_id, a_id) 
SELECT nextval('pgtraj.bursts_id_seq'), 1;
INSERT INTO pgtraj.bursts (b_id, b_name, a_id) 
SELECT nextval('pgtraj.bursts_id_seq'), 'burst2', 2;

INSERT INTO pgtraj.bursts (b_id, b_name, a_id) 
SELECT nextval('pgtraj.bursts_id_seq'), 'burst1',1;

INSERT INTO pgtraj.pgtrajs (p_id, p_name) VALUES (55, 'ltraj-one');
INSERT INTO pgtraj.pgtrajs (p_name) VALUES ( 'ltraj-two');

INSERT INTO pgtraj.steps (step) 
select st_makeline(a.geom, b.geom)::geography
FROM example_data.relocations_sorted AS a JOIN example_data.relocations_sorted AS b
ON a.gid + 1 = b.gid
WHERE a.gid = 5;

INSERT INTO pgtraj.steps (step) 
select b.geom::geography
FROM example_data.relocations_sorted AS b
WHERE b.gid = 8;


/* Infolocs table
 * 
 */
INSERT INTO pgtraj.infolocs (infoloc) VALUES ('{"a":1,"b":2}');
INSERT INTO pgtraj.infolocs (infoloc) VALUES ('{"a":3,"b":4}');
INSERT INTO pgtraj.infolocs (infoloc) VALUES ('{"accuracy":1.6,"land use":"forest", "activity":"foraging"}');
INSERT INTO pgtraj.infolocs (infoloc) VALUES ('{"accuracy":0.76,"land use":"meadow", "activity":"migrating"}');
INSERT INTO pgtraj.infolocs (infoloc) VALUES ('{"accuracy":0.71,"land use":"meadow", "activity":"migrating"}');

SELECT 
    infoloc->'accuracy' AS accuracy --get all values as json
FROM pgtraj.infolocs;

SELECT 
    infoloc->>'land use' AS land_use --get all values as text
FROM pgtraj.infolocs;

SELECT 
    json_each(infoloc)
FROM pgtraj.infolocs;

SELECT 
    json_object_keys(infoloc)
FROM pgtraj.infolocs;

-- check server encoding (should be UTF8)
SHOW SERVER_ENCODING;
-- check postgres version (should be >=9.3)
SELECT version();

-- insert steps
EXPLAIN ANALYZE
INSERT INTO pgtraj.steps (
    s_id,
    step,
    date,
    dt
) (
    SELECT 
        e.startgid AS s_id,
        e.step_geog AS step,
        e.time AS date,
        e.dt
    FROM example_data.steps e
);


/* Delete stuff
 */

DROP SEQUENCE pgtraj.animals_id_seq;
DROP SEQUENCE pgtraj.bursts_id_seq;
DROP SEQUENCE pgtraj.pgtrajs_id_seq;
DROP SEQUENCE pgtraj.pgtrajs_p_id_seq;
DROP SEQUENCE pgtraj.steps_id_seq;
DROP SEQUENCE pgtraj.steps_s_id_seq;
DROP SEQUENCE pgtraj.infolocs_i_id_seq;

DROP TYPE animals CASCADE;
