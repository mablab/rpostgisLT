#' Title
#'
#' @param conn 
#' @param schema 
#' @param pgtraj 
#' @param force Boolean. Drop and recreate the views if they already exist.
#'
#' @return
#' @export
#'
#' @examples
createShinyViews <- function(conn, schema, pgtraj, force=FALSE) {
    sview <- paste0("step_geometry_shiny_", pgtraj)
    bview <- paste0("all_burst_summary_shiny")
    schema_s <- DBI::dbQuoteString(conn, schema)
    # list tables in schema
    sql_query <- paste0("SELECT viewname FROM pg_views WHERE schemaname =", schema_s)
    relations <- DBI::dbGetQuery(conn, sql_query)$viewname
    # if force -> drop and recreate the views
    if(!(sview %in% relations)){
        rpostgisLT:::createShinyStepsView(conn, schema, pgtraj)
    } else if (!(bview %in% relations)) {
        rpostgisLT:::createShinyBurstsView(conn, schema)
    }
}

shinyViews(conn, "ibex_traj", "ibex")
