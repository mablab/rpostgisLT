##' Checks if the provided schema exists in the database, and creates
##' if it doesn't.
##'
##' Creates a 'traj' schema in the database by calling a SQL script 
##' from (./inst/sql/). The schema name defaults to 'traj'.
##' @title Check 'traj' schema.
##' @param conn Connection object created with RPostgreSQL.
##' @param schema Chaaracter string. Name of the schema that stores or
##'     will store the pgtraj data model.
##' @return TRUE on success
##' @author Balázs Dukai \email{balazs.dukai@@gmail.com}
##' @export
##' @examples
##' \dontrun{pgTrajSchema(conn, "traj_1")}
pgTrajSchema <- function(conn, name = "traj") {
    # [to stay consistent with RPostgreSQL] Create traj database
    # schema if it doesn't exist TODO Check if also all necessary
    # tables exist
    
    ## Check if PostGIS is installed
    suppressMessages(pgPostGIS(conn))
    ## Begin transaction block
    invisible(dbGetQuery(conn, "BEGIN TRANSACTION;"))
    ## Create schema
    pgSchema(conn, name, display = FALSE, exec = TRUE)
    ## Set DB search path for the schema
    current_search_path <- RPostgreSQL::dbGetQuery(conn, "SHOW search_path;")
    query <- paste0("SET search_path TO ", name, ",public;")
    invisible(dbGetQuery(conn, query))
    ## SQL query to set up schema
    pgtraj_schema_file <- paste0(path.package("rpostgisLT"),
        "/sql/pgtraj_schema.sql")
    query <- paste(readLines(pgtraj_schema_file), collapse = "\n")
    invisible(dbGetQuery(conn, query))
    ## Reset DB search path to the public schema
    query <- paste0("SET search_path TO ", current_search_path, ";")
    invisible(RPostgreSQL::dbGetQuery(conn, query))

    ## Commit transaction block
    invisible(dbCommit(conn))

    message(paste("Schema", name, "successfully created in the database."))

    return(TRUE)
}
