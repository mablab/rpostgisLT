#!/bin/bash

# usage: ./restore-rpglt_data.sh <name of the DB>

dropdb $1
createdb $1
psql -d $1 -c "create extension postgis"
psql -d $1 -c "create schema example_data; create schema ibex_traj_materialized_bursts;"
gzip -dc ./rpglt_data.sql.gz | psql -d $1
gzip -dc ./ibex_traj_materialized_bursts.sql.gz | psql -d $1
