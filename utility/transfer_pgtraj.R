source("./utility/connect_db.R")

# connect to remote
conn <- cs(pw="gsoc", host = "ufl")

# connect to localhost
conn_local <- cs(pw="pw", host = "local", user = "user", dbname = "db")


# Transfer pgtraj -------------------------------------------------------------

library(rpostgisLT)

stork_2004 <- pgtraj2ltraj(conn, schema = "stork_traj", pgtraj = "2004")
ltraj2pgtraj(conn_local, ltraj = stork_2004, schema = "stork_traj",
             pgtraj = "2004", overwrite = TRUE)


# Create a pgtraj with infolocs -------------------------------------------

asPgtraj(conn, relocations_table = c("example_data", "stork_gps"),
          schema = "stork_traj", pgtrajs = "pgtraj", animals = "animal_id",
          bursts = "burst", info_cols = "gps_data_animals_id",
          relocations = "geom", timestamps = "acquisition_time",
          rids = "gps_data_animals_id")

# another one with 0 SRID
cap <- as.ltraj(xy = capreochiz[,c("x","y")], date = capreochiz$date,
                id = "Roe.Deer", typeII = TRUE, infolocs = capreochiz[,4:8])
ltraj2pgtraj(conn, cap, schema = "cap_traj", pgtraj = "cap")
schema <- "cap_traj"
pgtraj <- "cap"
createShinyView(conn, schema, pgtraj)


