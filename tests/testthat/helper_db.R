can_con <- function(x) {
    inherits(x, "PostgreSQLConnection")
}
conn_empty <- NULL
test_that("check utils", expect_false(can_con(conn_empty)))
try(conn_empty <- RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(),
                                   dbname = "rpglt_empty"))
# pg_verbosity <-
#     RPostgreSQL::dbGetQuery(conn, "SHOW log_error_verbosity;")[["log_error_verbosity"]]
# RPostgreSQL::dbSendQuery(conn, "SET log_error_verbosity TO 'terse';")
conn_data <- NULL
test_that("check utils", expect_false(can_con(conn_data)))
try(conn_data <- RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(),
                                         dbname = "rpglt_data"))

# attr(ibex_int_space, 'proj4string') <- CRS("+init=epsg:3395")
# ltraj2pgtraj(conn_data, ltraj = ibex_int_space, schema = "ibex_traj_materialized_bursts",
#              infolocs = TRUE)
# rpostgisLT:::createShinyBurstsView(conn_data,"ibex_traj_materialized_bursts")
# rpostgisLT:::createShinyStepsView(conn_data, "ibex_traj_materialized_bursts", "ibex_int_space")
