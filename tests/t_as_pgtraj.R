# 
# Author: bdukai
###############################################################################
# all variables stored with the raw data

as_pgtraj(conn, 
        schema = "traj_t2",
        relocations_table = "example_data.relocations_plus",
        pgtrajs = "id",
        animals = "animal",
        bursts = "burst",
        relocations = "geom",
        timestamps = "time",
        rid = "gid")

# variables provided manually
as_pgtraj(conn, 
        schema = "traj_t4",
        relocations_table = "example_data.reloc_medium", 
        pgtrajs = "medium",
        animals = "sea turtle",
        relocations = "geom",
        timestamps = "time",
        rid = "gid")

# trajectory Type I
as_pgtraj(conn, 
        schema = "traj_t4",
        relocations_table = "example_data.reloc_t1", 
        pgtrajs = "small",
        animals = "small animal",
        relocations = "geom",
        rid = "gid")




data(ibex)
ltraj2pgtraj(conn, ibex, "traj_9")
ibex_pg <- pgtraj2ltraj(conn, "traj_9", "ibex")

data(ibexraw)
ibex_raw <- rpostgisLT:::ld_opt(ibexraw)
attr(ibex_raw$date, "tzone") <- "Europe/Paris"
ibex_raw <- dl_opt(ibex_raw)
ltraj2pgtraj(conn, ibex_raw, "traj_t4")

as_pgtraj(conn, 
        schema = "traj_t3",
        relocations_table = "public.fires",
        pgtrajs = "fires",
        animals = "fires_a",
        bursts = "fires_b",
        relocations = "wkb_geometry",
        timestamp = "time",
        rid = "ogc_fid")

query <- paste0("SELECT ST_SRID(", relocations,
        ") FROM ", relocations_table," LIMIT 1;")

query <- "select st_srid(wkb_geometry) from public.fires limit 1;"
epsg <- RPostgreSQL::dbGetQuery(conn, query)[1,1]


pgTrajDB2TempT(conn, schema = "traj_t1", 
              relocations_table = "public.fires", 
              pgtrajs = "fires", animals = "fires_a", bursts = "fires_b", 
              relocations = "wkb_geometry", timestamps = "time", rids = "ogc_fid",
              epsg = 900914)

rpostgisLT:::pgTrajTempT(conn, "traj_5")
rpostgisLT:::pgTrajR2TempT(conn, "traj_5", dframe=ibex_raw, pgtraj="ibexraw", epsg=0)

rpostgisLT:::pgTrajParamsView(conn, "traj_6", "ibex", 0)
