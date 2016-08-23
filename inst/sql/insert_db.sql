/*
 * Populate the traj schema in case data was input from a database table
 */

/*prepares relocations for insert and passing data to other tables*/
WITH relocation_input AS (
    SELECT
        nextval('relocation_id_seq') AS id,
        geom,
        relocation_time,
        burst_name,
        animal_name,
        pgtraj_name,
        proj4string,
        time_zone,
        note
    FROM zgaqtsn_temp
    ORDER BY pgtraj_name, burst_name, relocation_time
    ),
insert_relocation AS (
    INSERT INTO relocation (id, geom, relocation_time)
    SELECT id, geom, relocation_time
    FROM relocation_input
    ),
--/*prepares steps for insert and passing data to other tables*/
step_input AS (
    SELECT
        nextval('step_id_seq') AS id,
        a.id AS relocation_id_1,
        b.id AS relocation_id_2,
        b.relocation_time - a.relocation_time AS dt,
        a.burst_name,
        a.pgtraj_name
    FROM relocation_input a
    LEFT OUTER JOIN LATERAL (SELECT c.id, c.relocation_time
                       FROM relocation_input c
                       WHERE a.relocation_time < c.relocation_time
                       AND a.burst_name = c.burst_name
                       AND a.pgtraj_name = c.pgtraj_name
                       ORDER BY c.relocation_time ASC
                       LIMIT 1
                      ) AS b 
    ON TRUE
    ),
insert_step AS (
    INSERT INTO step (id, relocation_id_1, relocation_id_2, dt)
    SELECT id, relocation_id_1, relocation_id_2, dt
    FROM step_input
    ),
insert_pgtraj AS (
    INSERT INTO pgtraj (pgtraj_name, proj4string, time_zone, note)
    SELECT DISTINCT pgtraj_name, proj4string, time_zone, note
    FROM relocation_input
    RETURNING id, pgtraj_name
    ),
insert_animal_burst AS (
    INSERT INTO animal_burst (burst_name, animal_name, pgtraj_id)
    SELECT DISTINCT a.burst_name, a.animal_name, b.id
    FROM relocation_input AS a
    JOIN insert_pgtraj AS b ON b.pgtraj_name = a.pgtraj_name
    RETURNING id, burst_name
)
INSERT INTO s_i_b_rel (step_id, animal_burst_id)
SELECT a.id, b.id
FROM step_input AS a
JOIN insert_animal_burst AS b ON a.burst_name = b.burst_name;

