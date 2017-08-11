can_con <- function(x)
    inherits(x, "PostgreSQLConnection")
conn <- NULL
test_that("check utils", expect_false(can_con(conn)))
try(conn_empty <- RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(),
                                   dbname = "rpglt_empty"))
# pg_verbosity <-
#     RPostgreSQL::dbGetQuery(conn, "SHOW log_error_verbosity;")[["log_error_verbosity"]]
# RPostgreSQL::dbSendQuery(conn, "SET log_error_verbosity TO 'terse';")
try(conn_data <- RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(),
                                         dbname = "rpglt_data"))