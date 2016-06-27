/*
 * Create views with minimum and maximum coordinates
 */
-- GEOMETRY
--select the point with the minimum x or minimum y value
CREATE VIEW example_data.xymin AS (
SELECT 
    rel.gid,
    rel.id,
    rel.geom,
    rel.geog,
    cont.xmin,
    cont.ymin
FROM 
    example_data.relocations_sorted AS rel,
    (
        SELECT
            gid,
            id,
            st_x AS xmin,
            st_y AS ymin
        FROM (SELECT gid,
                     id,
                     st_x(geom), 
                     st_y(geom)
              FROM example_data.relocations_sorted) AS xy
        WHERE xy.st_y in (SELECT min(st_y(geom)) ymin
                         FROM example_data.relocations_sorted
                         GROUP BY id
                         ) OR 
              xy.st_x in (SELECT min(st_x(geom)) xmin
                         FROM example_data.relocations_sorted
                         GROUP BY id
                         )
     ) AS cont
WHERE rel.gid = cont.gid
);

--select the point with the maximum x or maximum y value
CREATE VIEW example_data.xymax AS (
SELECT 
    rel.gid,
    rel.id,
    rel.geom,
    rel.geog,
    cont.xmax,
    cont.ymax
FROM 
    example_data.relocations_sorted AS rel,
    (
        SELECT
            gid,
            id,
            st_x AS xmax,
            st_y AS ymax
        FROM (SELECT gid,
                     id,
                     st_x(geom), 
                     st_y(geom)
              FROM example_data.relocations_sorted) AS xy
        WHERE xy.st_y in (SELECT max(st_y(geom)) ymax
                         FROM example_data.relocations_sorted
                         GROUP BY id
                         ) OR 
              xy.st_x in (SELECT max(st_x(geom)) xmax
                         FROM example_data.relocations_sorted
                         GROUP BY id
                         )
     ) AS cont
WHERE rel.gid = cont.gid
);



-- GEOGRAPHY latitude span
SELECT id, min(ymin), geog
FROM example_data.xymin
GROUP BY id, geog;



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

--TODO bla

/*
 * Calculate Root Mean Square Error for the geometry-geography differences (rounded)
 */
CREATE OR REPLACE VIEW example_data.geom_vs_geog AS
(
    SELECT 
        rmse.id,
        --round(rmse."RMSE_m"/1000, 2) AS "RMSE_km",
        round(rmse."ME_m"/1000, 2) AS "ME_km",
        s.avg_step_length_km,
        s.sum_steps_km,
        t.total_length_km
    FROM 
    (
        SELECT 
            diff.id, 
            --round(sqrt(sum(diff.d^2)/count(*))::numeric, 2) AS "RMSE_m",
            round(sum(diff.d)::numeric/count(*), 2) AS "ME_m"
        FROM (
            SELECT 
                id,
                length_geom-length_geog AS d
            FROM example_data.steps) AS diff
        GROUP BY diff.id) AS rmse,
    (
        SELECT 
            id, 
            round(sum(steps.length_geog)::numeric/1000::numeric, 2) AS sum_steps_km,
            round(avg(steps.length_geog)::NUMERIC/1000::numeric, 2) AS avg_step_length_km
        FROM example_data.steps AS steps
        GROUP BY id) AS s,
    (
        SELECT 
            id, 
            round(st_length(st_makeline(rel.geom)::geography)::numeric/1000::numeric, 2) AS total_length_km
        FROM example_data.relocations_sorted AS rel
        GROUP BY id) AS t
    WHERE 
        rmse.id = s.id AND
        rmse.id = t.id
);


SELECT 
    st_ymax(st_envelope(rel.geom)) - st_ymin(st_envelope(rel.geom)) AS ylength
FROM example_data.relocations_sorted AS rel
WHERE id = 'small';

SELECT st_srid(geom)
FROM example_data.relocations_sorted;

SELECT 
    id,
    st_xmax(st_setsrid(st_extent(rel.geom), 4326) - st_xmin(st_setsrid(st_extent(rel.geom), 4326)) AS xdist,
    st_ymax(st_setsrid(st_extent(rel.geom), 4326)) - st_ymin(st_setsrid(st_extent(rel.geom), 4326)) AS ydist
FROM example_data.relocations_sorted AS rel
WHERE id = 'small';

SELECT 
    id,
    st_xmax(st_setsrid(st_extent(rel.geom), 4326) - st_xmin(st_setsrid(st_extent(rel.geom), 4326)) AS xdist,
    st_ymax(st_setsrid(st_extent(rel.geom), 4326)) - st_ymin(st_setsrid(st_extent(rel.geom), 4326)) AS ydist
FROM example_data.relocations_sorted AS rel
WHERE id = 'small';