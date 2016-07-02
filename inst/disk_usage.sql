/* These queries report the size of the relations in- and across database
 * Reference: https://wiki.postgresql.org/wiki/Disk_Usage
 */


/*
 *  Finding the size of your biggest relations 
 */
SELECT nspname || '.' || relname AS "relation",
    pg_size_pretty(pg_relation_size(C.oid)) AS "size"
FROM pg_class C
LEFT JOIN pg_namespace N ON (N.oid = C.relnamespace)
WHERE nspname NOT IN ('pg_catalog', 'information_schema')
ORDER BY pg_relation_size(C.oid) DESC
LIMIT 20;



/*
 * Create steps table with relocations (POINTS) only and with steps (LINESTRING)
 */
CREATE TABLE pgtraj.relocations2 (
    r_id        serial      PRIMARY KEY,
    relocation  geography   NOT NULL,
    "date"      timestamptz DEFAULT NULL
);

CREATE TABLE pgtraj.steps2 (
    s_id        serial   PRIMARY KEY,
    step        geography   NOT NULL,
    "date"      timestamptz DEFAULT NULL
);

-- insert steps
EXPLAIN ANALYZE
INSERT INTO pgtraj.steps2 (
    --s_id,
    step,
    date
) (
    SELECT 
        --e.startgid AS s_id,
        e.step_geog AS step,
        e.time AS date
    FROM example_data.steps e
);

--DROP TABLE pgtraj.steps CASCADE;

EXPLAIN ANALYZE
INSERT INTO pgtraj.relocations2 (
    --r_id,
    relocation,
    date
) (
    SELECT 
        --e.gid AS s_id,
        e.geog AS step,
        e.time AS date
    FROM example_data.relocations_sorted e
);

VACUUM (FULL, VERBOSE, ANALYZE) pgtraj.relocations2;
VACUUM (FULL, VERBOSE, ANALYZE) pgtraj.steps2;

SELECT st_geometrytype(relocation::geometry)
FROM pgtraj.relocations;
SELECT st_geometrytype(step::geometry)
FROM pgtraj.steps;


