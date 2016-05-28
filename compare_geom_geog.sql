-- create view for the contiental trajectory
CREATE VIEW example_data.continental AS (
SELECT * 
FROM example_data.relocations
WHERE id='continental');

-- GEOMETRY
--select the point with the minimum Y value
CREATE VIEW example_data.cont_ymin AS (
SELECT rel.gid, rel.geom, rel.geog 
FROM example_data.continental AS rel, (
    SELECT gid, st_x, st_y
    FROM (SELECT gid, 
                 st_x(geom), 
                 st_y(geom)
          FROM example_data.continental) AS xy
    WHERE xy.st_y = (SELECT min(st_y(geom))
                     FROM example_data.continental)
     ) AS cont
WHERE rel.gid = cont.gid
);

--select the point with the maximum Y value
CREATE VIEW example_data.cont_ymax AS (
SELECT rel.gid, rel.geom, rel.geog
FROM example_data.continental AS rel, (
    SELECT gid, st_x, st_y
    FROM (SELECT gid, 
                 st_x(geom), 
                 st_y(geom)
          FROM example_data.continental) AS xy
    WHERE xy.st_y = (SELECT max(st_y(geom))
                     FROM example_data.continental)
    ) AS cont
WHERE rel.gid = cont.gid
);


SELECT st_srid(cont_ymax.geom) 
FROM example_data.cont_ymax;

-- GEOMETRY distance (srid 3395)
SELECT st_distance(st_transform(cont_ymax.geom, 3395), st_transform(cont_ymin.geom,3395)) 
FROM example_data.cont_ymax, example_data.cont_ymin;
--result: 4360838.681311944

-- GEOGRAPHY distance
SELECT st_distance(cont_ymax.geog, cont_ymin.geog) 
FROM example_data.cont_ymax, example_data.cont_ymin;
--result: 4124174.970921178

SELECT st_distance(st_transform(a.geom, 3395), st_transform(b.geom,3395))
FROM (select geom from example_data.continental where gid=14894) AS a,
(select geom from example_data.continental where gid=14892) AS b;



/*
 * Calculate Root Mean Square Error for the geometry-geography differences (rounded)
 */
CREATE OR REPLACE VIEW geom_vs_geog AS
(
    SELECT 
        rmse.id,
        rmse."RMSE_m",
        s.sum_steps_km,
        t.total_length_km
    FROM 
    (
        SELECT 
            diff.id, 
            round(sqrt(sum(diff.d^2)/count(*))::numeric, 2) AS "RMSE_m"
        FROM (
            SELECT 
                id,
                length_geom-length_geog AS d
            FROM example_data.steps) AS diff
        GROUP BY diff.id) AS rmse,
    (
        SELECT id, round(sum(steps.length_geog)::numeric/1000::numeric, 2) AS sum_steps_km
        FROM example_data.steps AS steps
        GROUP BY id) AS s,
    (
        SELECT id, round(st_length(st_makeline(rel.geom)::geography)::numeric/1000::numeric, 2) AS total_length_km
        FROM example_data.relocations AS rel
        GROUP BY id) AS t
    WHERE 
        rmse.id = s.id AND
        rmse.id = t.id
);
