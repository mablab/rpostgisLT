# 
# Author: bdukai
###############################################################################
library(RPostgreSQL)
library(rpostgis)

source("./R/as_pgtraj.r")

# all variables stored with the raw data
as_pgtraj(conn, 
        schema = "traj_t1",
        relocation_data = "example_data.relocations_plus", 
        pgtrajs = "id",
        animals = "animal",
        bursts = "burst",
        relocations = "geom",
        timestamp = "time",
        rid = "gid")

# variables provided manually
as_pgtraj(conn, 
        schema = "traj_t2",
        relocation_data = "example_data.reloc_medium", 
        pgtrajs = "medium",
        animals = "sea turtle",
        relocations = "geom",
        timestamp = "time",
        rid = "gid")

# trajectory Type I
as_pgtraj(conn, 
        schema = "traj_t3",
        relocation_data = "example_data.reloc_t1", 
        pgtrajs = "small",
        animals = "small animal",
        relocations = "geom",
        rid = "gid")

