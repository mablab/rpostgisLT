-- VIEW TO compute the step parameters
/*CREATE MATERIALIZED VIEW pgtraj.step_parameters AS
SELECT 
    s_id,
    x,
    y,
    time AS date,
    dx,
    dy,
    st_length(step) AS dist,
    dtime AS dt,
    r2n,
    abs_angle,
    rel_angle,
    b_id
FROM pgtraj.steps*/

CREATE MATERIALIZED VIEW pgtraj.step_parameters AS
SELECT 
    s_id,
    st_x(st_startpoint(step::geometry)) AS x, 
    st_y(st_startpoint(step::geometry)) AS y,
    date,
    ST_Distance_Sphere(st_startpoint(step::geometry),
                st_makepoint(st_x(st_endpoint(step::geometry)), 
                             st_y(st_startpoint(step::geometry)))) AS dx,
    ST_Distance_Sphere(st_makepoint(st_x(st_endpoint(step::geometry)), 
                             st_y(st_startpoint(step::geometry))),
            st_endpoint(step::geometry)) AS dy,
    st_length(step) AS dist,
    dt
FROM pgtraj.steps;


SELECT 
        b.startgid,
        b.id,
        degrees(
               st_azimuth(
                          st_startpoint(b.step_geog::geometry),
                          st_endpoint(b.step_geog::geometry)
                          )
               -
               st_azimuth(
                          st_startpoint(a.step_geog::geometry),
                          st_endpoint(a.step_geog::geometry)
                         )
               )
FROM example_data.steps AS a INNER JOIN example_data.steps AS b 
ON a.startgid = b.startgid + 1
AND a.id = b.id;



/* Use a spheroid for calculating dy, dy
 * WGS 84 spheroid
 */
CREATE VIEW example_data.step_param_test AS
SELECT 
    startgid AS s_id,
    st_x(st_startpoint(step_geog::geometry)) AS x, 
    st_y(st_startpoint(step_geog::geometry)) AS y,
    time AS date,
    ST_Distance_Spheroid(
                        st_startpoint(step_geog::geometry),
                        st_setSRID(
                                   st_makepoint(
                                                st_x(st_endpoint(step_geog::geometry)), 
                                                st_y(st_startpoint(step_geog::geometry))
                                               ),
                                   4326
                                   ),
                        'SPHEROID["WGS 84",6378137,298.257223563]'
                         ) AS dx,
    ST_Distance_Spheroid(
                       st_setSRID(
                                  st_makepoint(
                                               st_x(st_endpoint(step_geog::geometry)), 
                                               st_y(st_startpoint(step_geog::geometry))
                                               ),
                                  4326
                                  ),
                       st_endpoint(step_geog::geometry),
                       'SPHEROID["WGS 84",6378137,298.257223563]'
                       ) AS dy,
    st_length(step_geog) AS dist,
    dt
FROM example_data.steps;

-- median error with using st_distance_spheroid when calculating dx, dy
-- -0.46
SELECT sum(error) / count(*) 
FROM (
    SELECT sqrt(dx^2 + dy^2) AS test_dist, 
           dist,
           sqrt(dx^2 + dy^2) - dist AS error
    FROM example_data.step_param_test
) AS foo;


/* Use ST_Distance_Sphere when calculating dx, dy
 * Using sphere, not spheroid
 */
CREATE OR REPLACE VIEW example_data.step_param_t_sphere AS
SELECT 
    startgid AS s_id,
    st_x(st_startpoint(step_geog::geometry)) AS x, 
    st_y(st_startpoint(step_geog::geometry)) AS y,
    time AS date,
    ST_Distance_Sphere(
                       st_startpoint(step_geog::geometry),
                       st_setSRID(
                                   st_makepoint(
                                                st_x(st_endpoint(step_geog::geometry)), 
                                                st_y(st_startpoint(step_geog::geometry))
                                                ),
                                  4326
                                  )
                       ) AS dx,
    ST_Distance_Sphere(
                        st_setSRID(
                                    st_makepoint(
                                                 st_x(st_endpoint(step_geog::geometry)), 
                                                 st_y(st_startpoint(step_geog::geometry))
                                                 ),
                                   4326
                                   ),
                        st_endpoint(step_geog::geometry)
                       ) AS dy,
    st_length(step_geog) AS dist,
    dt
FROM example_data.steps;

-- median error with using st_distance_sphere when calculating dx, dy
-- 3.40
SELECT sum(error) / count(*) 
FROM (
    SELECT sqrt(dx^2 + dy^2) AS test_dist, 
           dist,
           sqrt(dx^2 + dy^2) - dist AS error
    FROM example_data.step_param_t_sphere
) AS foo;


 