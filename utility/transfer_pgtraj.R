source("./utility/connect_db.R")

# connect to remote
conn <- cs(pw="gsoc", host = "ufl")

# connect to localhost
conn_local <- cs(pw="pw", host = "local", user = "user", dbname = "db")


# Transfer pgtraj -------------------------------------------------------------

library(rpostgisLT)

stork_2004 <- pgtraj2ltraj(conn, schema = "stork_traj", pgtraj = "2004")
ltraj2pgtraj(conn_local, ltraj = stork_2004, schema = "stork_traj",
             overwrite = TRUE)