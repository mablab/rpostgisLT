/* Calculate step parameters pgtraj v6
 */
SET search_path TO traj_t1,public;
SET search_path TO "$user",public;

--CREATE OR REPLACE VIEW example_data.step_param AS
SELECT 
    r_rowname,
    st_x(s.reloc1) AS x, 
    st_y(s.reloc1) AS y,
    s.date,
    ST_Distance(
               st_startpoint(s.step),
               st_makepoint(
                            st_x(st_endpoint(s.step)), 
                            st_y(st_startpoint(s.step))
                            )
                ) AS dx,
    ST_Distance(
                st_makepoint(
                             st_x(st_endpoint(s.step)), 
                             st_y(st_startpoint(s.step))
                             ),
                st_endpoint(s.step)
               ) AS dy,
    st_length(s.step) AS dist,
    s.dt,
    ST_Distance(startp.reloc1, s.reloc1) AS r2n,
    atan2(st_y(s.reloc1), st_x(s.reloc1)) AS abs_angle,
    (
        ST_Azimuth(st_startpoint(s2.step), st_endpoint(s2.step)) -
        ST_Azimuth(st_startpoint(s.step), st_endpoint(s.step))
    ) AS rel_angle,
    b.b_name AS burst,
    a.a_name AS id,
    p.p_name AS pgtraj
FROM steps AS s 
INNER JOIN steps AS s2 ON s.s_id + 1 = s2.s_id
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
            WHERE p_name LIKE 'ibex'
            GROUP BY s_rel.b_id
        ) AS m
        JOIN steps AS s ON s.s_id = m.s_id
    ) AS startp ON startp.b_id = s_rel.b_id
WHERE p_name LIKE 'ibex';
