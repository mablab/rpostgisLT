
CREATE VIEW example_data.continental AS (
SELECT * 
FROM example_data.relocations
WHERE id='continental');

-- GEOMETRY
--select the point with the minimum Y value
CREATE VIEW example_data.cont_ymin AS (
SELECT geom 
FROM example_data.continental, (
    SELECT gid, st_x, st_y
    FROM (SELECT gid, 
                 st_x(geom), 
                 st_y(geom)
          FROM example_data.continental) AS xy
    WHERE xy.st_y = (SELECT min(st_y(geom))
                     FROM example_data.continental)
     ) AS cont
WHERE example_data.continental.gid = cont.gid
);

--select the point with the maximum Y value
CREATE VIEW example_data.cont_ymax AS (
SELECT geom
FROM example_data.continental, (
    SELECT gid, st_x, st_y
    FROM (SELECT gid, 
                 st_x(geom), 
                 st_y(geom)
          FROM example_data.continental) AS xy
    WHERE xy.st_y = (SELECT max(st_y(geom))
                     FROM example_data.continental)
    ) AS cont
WHERE example_data.continental.gid = cont.gid
);


SELECT st_srid(cont_ymax.geom) 
FROM example_data.cont_ymax;

SELECT st_distance(cont_ymax.geom, cont_ymin.geom) 
FROM example_data.cont_ymax, example_data.cont_ymin;


