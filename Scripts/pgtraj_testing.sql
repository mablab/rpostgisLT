SELECT schemaname FROM pg_catalog.pg_tables WHERE tablename LIKE 'spatial_ref_sys';

SELECT srid FROM public.spatial_ref_sys WHERE proj4text ILIKE '%+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs%'; --should give 3395
SELECT proj4text FROM public.spatial_ref_sys WHERE auth_srid = 3395;
SELECT srtext FROM public.spatial_ref_sys WHERE auth_srid = 3395;

SELECT srid FROM public.spatial_ref_sys WHERE srtext ILIKE '%PROJCS["Mercator",GEOGCS["GCS_WGS_1984",DATUM["D_WGS_1984",SPHEROID["WGS_1984",6378137,298.257223563]],PRIMEM["Greenwich",0],UNIT["Degree",0.017453292519943295]],PROJECTION["Mercator"],PARAMETER["central_meridian",0],PARAMETER["false_easting",0],PARAMETER["false_northing",0],UNIT["Meter",1],PARAMETER["standard_parallel_1",0.0]]%';
PROJCS["WGS 84 / World Mercator",GEOGCS["WGS 84",DATUM["WGS_1984",SPHEROID["WGS 84",6378137,298.257223563,AUTHORITY["EPSG","7030"]],AUTHORITY["EPSG","6326"]],PRIMEM["Greenwich",0,AUTHORITY["EPSG","8901"]],UNIT["degree",0.0174532925199433,AUTHORITY["EPSG","9122"]],AUTHORITY["EPSG","4326"]],PROJECTION["Mercator_1SP"],PARAMETER["central_meridian",0],PARAMETER["scale_factor",1],PARAMETER["false_easting",0],PARAMETER["false_northing",0],UNIT["metre",1,AUTHORITY["EPSG","9001"]],AXIS["Easting",EAST],AXIS["Northing",NORTH],AUTHORITY["EPSG","3395"]]
PROJCS["WGS 84 / World Mercator",GEOGCS["WGS 84",DATUM["WGS_1984",SPHEROID["WGS 84",6378137,298.257223563,AUTHORITY["EPSG","7030"]],AUTHORITY["EPSG","6326"]],PRIMEM["Greenwich",0,AUTHORITY["EPSG","8901"]],UNIT["degree",0.01745329251994328,AUTHORITY["EPSG","9122"]],AUTHORITY["EPSG","4326"]],UNIT["metre",1,AUTHORITY["EPSG","9001"]],PROJECTION["Mercator_1SP"],PARAMETER["central_meridian",0],PARAMETER["scale_factor",1],PARAMETER["false_easting",0],PARAMETER["false_northing",0],AUTHORITY["EPSG","3395"],AXIS["Easting",EAST],AXIS["Northing",NORTH]]


SELECT * FROM public.spatial_ref_sys WHERE proj4text LIKE '%+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs%';

SELECT 3.110534542240202426910::numeric * 4.146736776456236839294::numeric;

SELECT extract(timezone FROM date)/3600.0 AS offset_from_UTC
FROM traj_est.steps
LIMIT 1;

SELECT date
FROM traj_est.steps
LIMIT 1;

CREATE TABLE test_tz (id serial, date timestamptz);
INSERT INTO test_tz (date) VALUES ('2003-06-01 00:00:56 CET'); -- timezone is ignored in case of invalid input
INSERT INTO test_tz (date) VALUES ('2003-06-01 00:00:56 EST'); -- timezone is ignored in case of invalid input
INSERT INTO test_tz (date) VALUES ('2003-06-01 00:00:56 -5:00');
INSERT INTO test_tz (date) VALUES ('2003-06-01 00:00:56 UTC');
SELECT extract(timezone FROM date)/3600.0 AS offset_from_UTC
FROM test_tz;

SELECT '2003-06-01 00:00:56 Europe/Paris'::timestamptz;

SELECT tablename FROM pg_tables WHERE schemaname='public';


DROP TABLE traj_test7.animals CASCADE;

SELECT * FROM pg_locks pl LEFT JOIN pg_stat_activity psa
    ON pl.pid = psa.pid;

