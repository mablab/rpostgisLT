# 
# Author: bdukai
###############################################################################
# all variables stored with the raw data
as_pgtraj(conn, 
        schema = "traj_t4",
        relocation_data = "example_data.relocations_plus",
        pgtrajs = "id",
        animals = "animal",
        bursts = "burst",
        relocations = "geom",
        timestamp = "time",
        rid = "gid")

# variables provided manually
as_pgtraj(conn, 
        schema = "traj_t4",
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

ltraj2pgtraj(conn, ibex, "traj_t2")
ibex_pg <- pgtraj2ltraj(conn, "traj_t2", "ibex")

data(ibexraw)
ibex_raw <- ld_opt(ibexraw)
attr(ibex_raw$date, "tzone") <- "Europe/Paris"
ibex_raw <- dl_opt(ibex_raw)
ltraj2pgtraj(conn, ibex_raw, "traj_t4")

make_pgtraj_schema(conn, "traj_t2")

as_pgtraj(conn, 
        schema = "traj_t3",
        relocation_data = "public.fires",
        pgtrajs = "fires",
        animals = "fires_a",
        bursts = "fires_b",
        relocations = "wkb_geometry",
        timestamp = "time",
        rid = "ogc_fid")

query <- paste0("SELECT ST_SRID(", relocations,
        ") FROM ", relocation_data," LIMIT 1;")

query <- "select st_srid(wkb_geometry) from public.fires limit 1;"
epsg <- dbGetQuery(conn, query)[1,1]

make_relocs_temp(conn, "traj_t1")

DB2relocs_temp(conn, schema = "traj_t1", 
              relocation_table = "public.fires", 
              pgtrajs = "fires", animals = "fires_a", bursts = "fires_b", 
              relocation_geom = "wkb_geometry", timestamps = "time", rids = "ogc_fid",
              epsg = 900914)
 