#' pgTrajVacuum

#' Performs a VACUUM (garbage-collect and optionally analyze) on all the
#' tables of a \code{traj} schema.
#' 
#' @author Balázs Dukai \email{balazs.dukai@@gmail.com}
#' 
#' @param conn Connection object created with RPostgreSQL
#' @param schema String. Name of the schema that stores or will store the pgtraj data model.
#' @param full Logical. Whether to perform a "full" vacuum, which can reclaim
#'     more space, but takes much longer and exclusively locks the table.
#' @param verbose Logical. Whether to print a detailed vacuum
#'     activity report for each table.
#' @param analyze Logical. Whether to update statistics used by the
#'     planner to determine the most efficient way to execute a query
#'     (default to \code{TRUE}).
#' @return \code{TRUE}
#' @examples
#' \dontrun{pgTrajVacuum(conn, "traj_1")}
#' @export
#' 
##############################################################################
pgTrajVacuum <- function(conn, schema, full = FALSE, verbose = FALSE,
        analyze = TRUE) {
    # Get all the tables in the schema
    sql_query <- paste0("SELECT tablename schemaname FROM pg_tables WHERE schemaname = '",schema,"';")
    tables <- dbGetQuery(conn, sql_query)
    
    for (tbl in tables$tablename) {
        dbVacuum(conn, name = tbl, full, verbose, analyze,
                display = TRUE, exec = TRUE)
    }
    
    # Return nothing
    return(TRUE)
}