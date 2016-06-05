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
 