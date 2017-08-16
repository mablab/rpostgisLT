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
