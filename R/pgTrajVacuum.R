#' pgtrajVacuum

#' VACUUM a pgtraj schema.
#' 
#' Performs a VACUUM (garbage-collect and optionally analyze) on all
#' the tables of a \code{traj} schema.
#'
#' @param conn Connection object created with RPostgreSQL
#' @param schema String. Name of the schema that stores or will store
#'     the pgtraj data model.
#' @param full Logical. Whether to perform a "full" vacuum, which can
#'     reclaim more space, but takes much longer and exclusively
#'     locks the table.
#' @param verbose Logical. Whether to print a detailed vacuum
#'     activity report for each table.
#' @param analyze Logical. Whether to update statistics used by the
#'     planner to determine the most efficient way to execute a query
#'     (default to \code{TRUE}).
#' @return \code{TRUE} on success.
#' @author Balázs Dukai \email{balazs.dukai@@gmail.com}
#' @export
#' @examples
#' \dontrun{
#'   # Vacuum analyze all tables in pgtraj schema with default name "traj"
#'   pgtrajVacuum(conn)
#' }

pgtrajVacuum <- function(conn, schema = "traj", full = FALSE, 
    verbose = FALSE, analyze = TRUE) {
    ## check PostgreSQL connection
    if (!inherits(conn, "PostgreSQLConnection")) {
        stop("'conn' should be a PostgreSQL connection.")
    }
    
    # Get all the tables in the schema
    sql_query <- paste0("SELECT tablename, schemaname FROM pg_tables WHERE schemaname = ", 
        dbQuoteString(conn, schema), ";")
    tables <- dbGetQuery(conn, sql_query)
    
    # just list main tables
    # pgtraj_tlist<-c('animal_burst','pgtraj','relocation','s_b_rel','step')
    # for (tbl in pgtraj_tlist) {
    
    for (tbl in tables$tablename) {
        dbVacuum(conn, name = c(schema, tbl), full, verbose, 
            analyze, display = TRUE, exec = TRUE)
    }
    
    ## Return TRUE on success
    return(TRUE)
}