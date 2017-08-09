# library(rpostgisLT)
can_con <- function(x) inherits(x, "PostgreSQLConnection")
conn <- NULL
test_that("check utils", expect_false(can_con(conn)))
try(conn <- RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(),
                                   dbname = "rpglt_empty"))