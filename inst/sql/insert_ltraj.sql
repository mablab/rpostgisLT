/*create sequences for uniting relocations with given and calculated coordinates*/
CREATE SEQUENCE loc1_id_seq INCREMENT BY 2
START WITH 1
OWNED BY zgaqtsn_temp.x;
CREATE SEQUENCE loc2_id_seq INCREMENT BY 2
START WITH 2
OWNED BY zgaqtsn_temp.x;

/*combines relocations with given(loc1) and calculated(loc2) coordinates and time*/
WITH r_output AS (
    SELECT
        x,
        y,
        date::timestamptz AS relocation_time,
        dx,
        dy,
        dt * INTERVAL '1 second' AS dt,
        "R2n" AS r2n,
        "rel.angle" AS rel_angle,
        id AS animal_name,
        burst AS burst_name,
		".burst_order"::integer AS burst_order,
        "r.row.names"::integer AS r_rowname,
        ".time_zone" AS time_zone,
        ".srid"::integer AS srid,
        ".proj4string" AS proj4string,
        ".pgtraj" AS pgtraj_name,
        ".note" AS note
    FROM zgaqtsn_temp
    ORDER BY burst_order, relocation_time
    ),
/*calculates relocation coordinates from x+dx etc. for obainting the 2nd 
 * relocation of a step and unites the computed values with the provided values*/
parsed_relocation AS (
    /*location 1 coordinates and timestamp*/
    SELECT
        nextval('loc1_id_seq') AS id,
        ST_SetSRID(ST_MakePoint(x, y), srid) AS geom,
        relocation_time,
        dt,
        r2n,
        rel_angle,
        r_rowname,
        burst_name
    FROM r_output
    UNION
    /*location 2 with computed coordinates and timestamp*/
    SELECT
        nextval('loc2_id_seq') AS id,
        ST_SetSRID(ST_MakePoint((x + dx), (y + dy)), srid) AS geom,
        relocation_time + dt AS relocation_time,
        dt,
        r2n,
        rel_angle,
        NULL AS r_rowname,
        burst_name
    FROM r_output
    ORDER BY id
    ),
/*prepares relocations for insert and passing data to other tables*/
relocation_input AS (
    SELECT
        nextval('relocation_id_seq') AS id,
        geom,
        relocation_time,
        dt,
        r2n,
        rel_angle,
        r_rowname,
        burst_name
    FROM parsed_relocation
    ),
insert_relocation AS (
    INSERT INTO relocation (id, geom, relocation_time)
    SELECT id, geom, relocation_time
    FROM relocation_input
    ),
/*prepares steps for insert and passing data to other tables*/
step_input AS (
    SELECT
        nextval('step_id_seq') AS id,
        a.id AS relocation_id_1,
        a.id + 1 AS relocation_id_2,
        a.dt,
        a.r_rowname,
        a.r2n,
        a.rel_angle,
        a.burst_name
    FROM (
        SELECT 
            id,
            dt,
            r_rowname,
            r2n,
            rel_angle,
            burst_name,
            (row_number() OVER (ORDER BY id)) % 2 AS rn
        FROM relocation_input
        ) AS a
    WHERE a.rn = 1
        AND a.id < (SELECT max(id) FROM relocation_input)
    ),
insert_step AS (
    INSERT INTO step (id, relocation_id_1, relocation_id_2, dt, r_rowname,
        r2n, rel_angle)
    SELECT id, relocation_id_1, relocation_id_2, dt, r_rowname,
        r2n, rel_angle
    FROM step_input
    ),
insert_pgtraj AS (
    INSERT INTO pgtraj (pgtraj_name, proj4string, time_zone, note)
    SELECT DISTINCT pgtraj_name, proj4string, time_zone, note
    FROM r_output
    RETURNING id, pgtraj_name
    ),
insert_animal_burst AS (
    INSERT INTO animal_burst (burst_name, animal_name, pgtraj_id)
    SELECT a.burst_name, a.animal_name, b.id
    FROM (SELECT DISTINCT pgtraj_name, burst_name, animal_name, burst_order from r_output) AS a
    JOIN insert_pgtraj AS b ON b.pgtraj_name = a.pgtraj_name
	ORDER BY b.id, a.burst_order 
    RETURNING id, burst_name
)
INSERT INTO s_i_b_rel (step_id, animal_burst_id)
SELECT a.id, b.id
FROM step_input AS a
JOIN insert_animal_burst AS b ON a.burst_name = b.burst_name;


