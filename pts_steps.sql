CREATE OR REPLACE FUNCTION upgis_utmzone_wgs84(geometry) 
RETURNS integer AS
$$ 
DECLARE 
    geomgeog geometry; 
    zone int; 
    pref int; 
BEGIN 
geomgeog:=ST_Transform(ST_Centroid($1),4326);                            --#1

IF (st_y(geomgeog))>0 THEN                                                  --#2
    pref:=32600;                                                         --#2
ELSE                                                                     --#2
    pref:=32700;                                                         --#2
END IF;                                                                  --#2
zone:=floor((ST_X(geomgeog)+180)/6)+1;                                   --#2

RETURN zone+pref;
END; 
$$ LANGUAGE 'plpgsql' immutable; 


DROP FUNCTION upgis_utmzone_wgs84(geometry);


SELECT upgis_utmzone_wgs84(cont.geom)
FROM (
    SELECT geom
    FROM example_data.continental
) AS cont;

-- table to store the steps
CREATE TABLE IF NOT EXISTS example_data.steps (
    startgid integer primary KEY,
    id text,
    step_geom geometry,
    step_geog geography,
    length_geom float4,
    length_geog float4
);


/*
 * Creates steps
 * Compares the length of the geometry and geography data types
 */
INSERT INTO example_data.steps
SELECT 
    steps.startgid,
    steps.id,
    step_geom,
    step_geog,
    st_length(st_transform(step_geom, 3395)) AS length_geom,
    st_length(step_geog) AS length_geog
FROM (
    SELECT 
        a.gid AS startgid,
        b.gid,
        a.id,
        st_makeline(a.geom, b.geom) AS step_geom,
        st_makeline(a.geom, b.geom)::geography AS step_geog
    FROM 
        example_data.relocations AS a
        INNER JOIN example_data.relocations AS b ON a.gid + 1 = b.gid
    ORDER BY 
        a.gid
) AS steps;


