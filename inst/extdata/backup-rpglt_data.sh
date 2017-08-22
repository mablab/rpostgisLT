#!/bin/bash

pg_dump --host localhost --port 5432 --username "bdukai" --no-password  --format plain --no-owner --encoding UTF8 --no-privileges --verbose --quote-all-identifiers --file "./inst/extdata/rpglt_data.sql" --table "example_data.reloc_medium" --table "example_data.reloc_t1" --table "example_data.relocations_plus" --table "example_data.test_line" --table "example_data.test_points" --table "example_data.test_polygons" --table "public.infoloc_test" --schema "ibex_traj_materialized_bursts" "rpglt_data"

gzip -f ./inst/extdata/rpglt_data.sql


