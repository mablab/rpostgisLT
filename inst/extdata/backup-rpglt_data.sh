#!/bin/bash

/usr/bin/pg_dump --host localhost --port 5432 --username "bdukai" --no-password  --format plain --section pre-data --section data --section post-data --encoding UTF8 --no-privileges --verbose --quote-all-identifiers --file "rpglt_data.sql" --table "example_data.albatross" --table "example_data.ibex" --table "example_data.reloc_medium" --table "example_data.reloc_t1" --table "example_data.relocations_plus" --table "example_data.test_line" --table "example_data.test_points_mixed" --table "example_data.test_points_multi" --table "example_data.test_points_multi_3395" --table "example_data.test_polygons" --table "public.infoloc_test" "rpglt_data"

/usr/bin/pg_dump --host localhost --port 5432 --username "bdukai" --no-password  --format plain --no-owner --section pre-data --section data --section post-data --encoding UTF8 --no-privileges --verbose --quote-all-identifiers --file "ibex_traj_materialized_bursts.sql" --schema "ibex_traj_materialized_bursts" "rpglt_data"

gzip -f rpglt_data.sql
gzip -f ibex_traj_materialized_bursts.sql

