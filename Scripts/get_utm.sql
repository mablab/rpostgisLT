/*
 * Get the matching UTM EPSG for the dataset
 */
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


SELECT upgis_utmzone_wgs84(cont.geom)
FROM (
    SELECT geom
    FROM example_data.continental
) AS cont;


DROP FUNCTION upgis_utmzone_wgs84(geometry);