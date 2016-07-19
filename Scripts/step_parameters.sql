/* Calculate step parameters pgtraj v6
 */
SET search_path TO params_test2,public;
SET search_path TO "$user",public;

CREATE OR REPLACE VIEW ib_rec_params AS 
SELECT
    t.r_rowname,
    t.x,
    t.y,
    t.date,
    t.dx,
    t.dy,
    t.dist,
    t.dt,
    t.r2n,
    atan2(t.dy, t.dx) AS abs_angle,
    t.rel_angle,
    t.id,
    t.burst,
    t.pgtraj
FROM 
(
SELECT 
    s.r_rowname,
    ST_x(s.reloc1) AS x, 
    ST_y(s.reloc1) AS y,
    s.date,
    ST_X(ST_endpoint(s.step)) - ST_X(ST_startpoint(s.step)) AS dx,
    ST_Y(ST_endpoint(s.step)) - ST_Y(ST_startpoint(s.step)) AS dy,
    ST_length(s.step) AS dist,
    extract(epoch FROM s.dt) AS dt,
    ST_Distance(startp.reloc1, s.reloc1) AS r2n,
    --atan2(ST_y(s.reloc1), ST_x(s.reloc1)) AS abs_angle,
    (
        ST_Azimuth(ST_startpoint(s2.step), ST_endpoint(s2.step)) -
        ST_Azimuth(ST_startpoint(s.step), ST_endpoint(s.step))
    ) AS rel_angle,
    a.a_name AS id,
    b.b_name AS burst,
    p.p_name AS pgtraj
FROM steps AS s 
LEFT JOIN steps AS s2 ON s.s_id + 1 = s2.s_id
JOIN s_i_b_rel AS s_rel ON s.s_id = s_rel.s_id
JOIN bursts AS b ON s_rel.b_id = b.b_id
JOIN animals AS a ON b.a_id = a.a_id
JOIN p_b_rel AS p_rel ON p_rel.b_id = b.b_id
JOIN pgtrajs AS p ON p_rel.p_id = p.p_id
JOIN 
    (
        SELECT 
            m.*,
            s.reloc1
        FROM 
        (
            SELECT
            min(s.s_id) AS s_id,
            s_rel.b_id
            FROM steps AS s
            JOIN s_i_b_rel AS s_rel ON s.s_id = s_rel.s_id
            JOIN p_b_rel AS p_rel ON p_rel.b_id = s_rel.b_id
            JOIN pgtrajs AS p ON p_rel.p_id = p.p_id
            WHERE p_name LIKE 'ib_rec'
            GROUP BY s_rel.b_id
        ) AS m
        JOIN steps AS s ON s.s_id = m.s_id
    ) AS startp ON startp.b_id = s_rel.b_id
WHERE p_name LIKE 'ib_rec'
) AS t;

-- x,y
SELECT
ST_x(relocation) AS x, 
ST_y(relocation) AS y
FROM params_test.relocs_temp;

-- date
SELECT extract(timezone FROM 
(SELECT date FROM params_test.relocs_temp LIMIT 1))/3600.0 AS offset_from_UTC;

SELECT extract(timezone FROM date)/3600.0 AS offset_from_UTC
FROM params_test.steps
LIMIT 1;


-- dx, dy
SELECT
    ST_Distance(
               ST_Startpoint(s.step)
               ST_SetSRID(
                          ST_Makepoint(
                                        ST_X(ST_endpoint(s.step)), 
                                        ST_Y(ST_startpoint(s.step))
                                      ),
                          0
                        )
                ) AS dx,
    ST_Distance(
                ST_SetSRID(
                           ST_Makepoint(
                                         ST_x(ST_endpoint(s.step)), 
                                         ST_y(ST_startpoint(s.step))
                                        ),
                            0
                            ),
                ST_endpoint(s.step)
               ) AS dy
FROM params_test.steps s

SELECT 
ST_X(ST_endpoint(s.step)) - ST_X(ST_startpoint(s.step)) AS dx,
ST_Y(ST_endpoint(s.step)) - ST_Y(ST_startpoint(s.step)) AS dy
FROM params_test2.steps s;

-- dist
SELECT
ST_length(s.step) AS dist
FROM params_test.steps s;

-- dt
SELECT extract(epoch FROM dt)
FROM params_test.steps s;

-- R2n
SELECT r2n FROM params_test.ib_params;

-- abs_angle
SELECT abs_angle FROM params_test.ib_params;

-- abs_angle
SELECT rel_angle FROM params_test.ib_params;