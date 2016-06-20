/*
 * Sort relocations on time
 */
CREATE TABLE if NOT EXISTS example_data.relocations_sorted (
    gid serial PRIMARY KEY,
    id text,
    "time" timestamptz,
    x float8,
    y float8,
    geom geometry,
    geog geography
);

INSERT INTO example_data.relocations_sorted (
    id,
    "time",
    x,
    y,
    geom,
    geog
)
SELECT 
    id,
    "time",
    x,
    y,
    geom,
    geog
FROM example_data.relocations
ORDER BY 
    id,
    "time";


/*
 * Create table to store steps
 */
CREATE TABLE IF NOT EXISTS example_data.steps (
    startgid integer primary KEY,
    id text,
    step_geom geometry,
    step_geog geography,
    length_geom float4,
    length_geog float4,
    "time" timestamptz,
    dt interval
);


/*
 * Creates steps
 * Compares the length of the geometry and geography data types
 */
-- id = small
INSERT INTO example_data.steps
SELECT 
    steps.startgid,
    steps.id,
    step_geom,
    step_geog,
    st_length(st_transform(step_geom, 3395)) AS length_geom,
    st_length(step_geog) AS length_geog,
    steps.time,
    steps.dt
FROM (
    SELECT 
        a.gid AS startgid,
        b.gid,
        a.id,
        st_makeline(a.geom, b.geom) AS step_geom,
        st_makeline(a.geom, b.geom)::geography AS step_geog,
        a.time,
        b.time - a.time AS dt
    FROM 
        example_data.relocations_sorted AS a
        INNER JOIN example_data.relocations_sorted AS b 
        ON a.gid + 1 = b.gid AND
        a.id = b.id
    WHERE a.id = 'small'
    ORDER BY a.gid
) AS steps;

-- id = medium
INSERT INTO example_data.steps
SELECT 
    steps.startgid,
    steps.id,
    step_geom,
    step_geog,
    st_length(st_transform(step_geom, 3395)) AS length_geom,
    st_length(step_geog) AS length_geog,
    steps.time, steps.dt
FROM (
    SELECT 
        a.gid AS startgid,
        b.gid,
        a.id,
        st_makeline(a.geom, b.geom) AS step_geom,
        st_makeline(a.geom, b.geom)::geography AS step_geog,
        a.time, b.time - a.time AS dt
    FROM 
        example_data.relocations_sorted AS a
        INNER JOIN example_data.relocations_sorted AS b 
        ON a.gid + 1 = b.gid AND
        a.id = b.id
    WHERE a.id = 'medium'
    ORDER BY a.gid
) AS steps;

-- id = large
INSERT INTO example_data.steps
SELECT 
    steps.startgid,
    steps.id,
    step_geom,
    step_geog,
    st_length(st_transform(step_geom, 3395)) AS length_geom,
    st_length(step_geog) AS length_geog,
    steps.time, steps.dt
FROM (
    SELECT 
        a.gid AS startgid,
        b.gid,
        a.id,
        st_makeline(a.geom, b.geom) AS step_geom,
        st_makeline(a.geom, b.geom)::geography AS step_geog,
        a.time, b.time - a.time AS dt
    FROM 
        example_data.relocations_sorted AS a
        INNER JOIN example_data.relocations_sorted AS b 
        ON a.gid + 1 = b.gid AND
        a.id = b.id
    WHERE a.id = 'large'
    ORDER BY a.gid
) AS steps;

-- id = large2
INSERT INTO example_data.steps
SELECT 
    steps.startgid,
    steps.id,
    step_geom,
    step_geog,
    st_length(st_transform(step_geom, 3395)) AS length_geom,
    st_length(step_geog) AS length_geog,
    steps.time, steps.dt
FROM (
    SELECT 
        a.gid AS startgid,
        b.gid,
        a.id,
        st_makeline(a.geom, b.geom) AS step_geom,
        st_makeline(a.geom, b.geom)::geography AS step_geog,
        a.time, b.time - a.time AS dt
    FROM 
        example_data.relocations_sorted AS a
        INNER JOIN example_data.relocations_sorted AS b 
        ON a.gid + 1 = b.gid AND
        a.id = b.id
    WHERE a.id = 'large2'
    ORDER BY a.gid
) AS steps;

-- id = continental
INSERT INTO example_data.steps
SELECT 
    steps.startgid,
    steps.id,
    step_geom,
    step_geog,
    st_length(st_transform(step_geom, 3395)) AS length_geom,
    st_length(step_geog) AS length_geog,
    steps.time, steps.dt
FROM (
    SELECT 
        a.gid AS startgid,
        b.gid,
        a.id,
        st_makeline(a.geom, b.geom) AS step_geom,
        st_makeline(a.geom, b.geom)::geography AS step_geog,
        a.time, b.time - a.time AS dt
    FROM 
        example_data.relocations_sorted AS a
        INNER JOIN example_data.relocations_sorted AS b 
        ON a.gid + 1 = b.gid AND
        a.id = b.id
    WHERE a.id = 'continental'
    ORDER BY a.gid
) AS steps;

DROP TABLE example_data.steps CASCADE;

