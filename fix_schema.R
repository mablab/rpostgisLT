drv <<- dbDriver("PostgreSQL")
conn <- dbConnect(drv, user="bdukai", password="ERBAgoNd1#", dbname="rpostgisLT",
        host="localhost")
dbDisconnect(conn)


pgTrajSchema(conn, "schema_t1")

rpostgisLT:::pgTrajTempT(conn, "schema_t1")

rpostgisLT:::pgTrajDB2TempT(conn, schema="schema_t1", 
        relocations_table = "example_data.relocations_plus",
        pgtrajs = "id", animals = "animal",
        bursts = "burst", relocations = "geom", timestamps = "time", 
        rids = "gid", srid = 4326)

dbSendQuery(conn, "DROP SCHEMA schema_t1 CASCADE;")

pgTrajSchema(conn, "schema_t2")

rpostgisLT:::pgTrajTempT(conn, "schema_t2")

rpostgisLT:::pgTrajDB2TempT(conn, schema="schema_t2", 
        relocations_table = "example_data.reloc_medium",
        pgtrajs = "medium", animals = "migrating animal",
        bursts = "burst1", relocations = "geom", timestamps = "time", 
        rids = "gid", srid = 4326)

dbSendQuery(conn, "DROP SCHEMA schema_t2 CASCADE;")

# Insert ltraj
data(ibexraw)
pgTrajSchema(conn, "schema_t3")
rpostgisLT:::pgTrajTempT(conn, "schema_t3")
ib_df <- rpostgisLT:::ld_opt(ibexraw)
rpostgisLT:::pgTrajR2TempT(conn, schema = "schema_t3", dframe = ib_df,
        pgtraj = "ibexraw", srid = 0)
dbSendQuery(conn, "DROP SCHEMA schema_t3 CASCADE;")

data(ibex)
pgTrajSchema(conn, "schema_t4")
rpostgisLT:::pgTrajTempT(conn, "schema_t4")
ib_df <- rpostgisLT:::ld_opt(ibex)
rpostgisLT:::pgTrajR2TempT(conn, schema = "schema_t4", dframe = ib_df,
        pgtraj = "ibex", srid = 0)
dbSendQuery(conn, "DROP SCHEMA schema_t4 CASCADE;")

# Insert from database
as_pgtraj


