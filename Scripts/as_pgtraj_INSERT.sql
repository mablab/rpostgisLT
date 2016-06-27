
/* as_pgtraj.r
 * Insert into relocs_temp
 */
CREATE TABLE relocs_temp (
    r_id    integer,
    relocation     geometry,
    date    timestamptz,
    b_name      text,
    a_name      text,
    p_name      text
);

INSERT INTO relocs_temp (r_id, relocation, date)
SELECT gid, geom::geometry, time
FROM example_data.relocations_plus
ORDER BY time;

UPDATE relocs_temp
SET p_name = a.id
FROM (
    SELECT gid, id
    FROM example_data.relocations_plus
) a
WHERE r_id = a.gid;

UPDATE relocs_temp
SET a_name = a.animal
FROM (
    SELECT gid, animal
    FROM example_data.relocations_plus
) a
WHERE r_id = a.gid;

UPDATE relocs_temp
SET b_name = a.burst
FROM (
    SELECT gid, burst
    FROM example_data.relocations_plus
) a
WHERE r_id = a.gid;

DROP TABLE relocs_temp;

/* as_pgtraj.r
 * Insert pgtrajs, animals, bursts
 */
INSERT INTO pgtrajs (p_name)
SELECT DISTINCT p_name
FROM relocs_temp;

INSERT INTO animals (a_name)
SELECT DISTINCT a_name
FROM relocs_temp;

INSERT INTO bursts (b_name, a_id)
SELECT DISTINCT a.b_name, b.a_id
FROM relocs_temp a JOIN animals b
ON a.a_name = b.a_name;

INSERT INTO p_b_rel (p_id, b_id)
SELECT DISTINCT b.p_id, b_id
FROM relocs_temp a 
    JOIN pgtrajs b
        ON a.p_name = b.p_name
    JOIN bursts c
        ON a.b_name = c.b_name;

-- insert steps in R loop

INSERT INTO s_i_b_rel (s_id, b_id)
SELECT a.s_id, b.b_id
FROM steps a, bursts b
WHERE a.b_name = 'migrating animal' 
AND b.b_name = 'migrating animal'; 

