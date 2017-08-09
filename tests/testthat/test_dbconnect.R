library(rpostgisLT)
library(testthat)
context("rpostgisLT: dbconnect")

can_con <- function(x) inherits(x, "PostgreSQLConnection")

db_drop_table_schema <- function(conn, schema, table = NULL) {
    if (is.null(table)) {
        table <- paste(c("public", schema), collapse = ".")
    } else {
        table <- paste(c(schema, table), collapse = ".")
    }
    DBI::dbSendQuery(pg, paste("DROP TABLE ", table, " CASCADE;"))
}

pg <- NULL
test_that("check utils", expect_false(can_con(pg)))
try(pg <- RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(),
                                 dbname = "rpglt_empty"), silent=TRUE)

test_that("can connect to db",{
    skip_if_not(can_con(pg), "could not connect to postgis database")
})