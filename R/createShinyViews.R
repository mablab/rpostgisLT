#' Create database views that are suitable for explorePgtraj()
#' 
#' It is expected that *all* pgtrajes are projected in the schema in order to
#' run.
#'
#' @param conn DBI::DBIConnection
#' @param schema String. Schema name.
#' @param pgtraj String. Pgtraj name.
#' @param force Boolean. Drop and recreate the views if they already exist.
#'
#' @return nothing
#' @export
#'
#' @examples
#' \dontrun{
#' createShinyViews(conn, schema="ibex_traj", pgtraj="ibex", force=TRUE)
#' }
createShinyViews <- function(conn, schema, pgtraj, force = FALSE) {
    # step view
    sview <- paste0("step_geometry_shiny_", pgtraj)
    # burst view
    bview <- paste0("all_burst_summary_shiny")
    
    schema_s <- DBI::dbQuoteString(conn, schema)
    # list tables in schema
    sql_query <-
        paste0("SELECT matviewname FROM pg_matviews WHERE schemaname =",
               schema_s,";")
    relations <- DBI::dbGetQuery(conn, sql_query)$matviewname
    # if force==TRUE -> drop and recreate the views
    if (!(sview %in% relations)) {
        createShinyStepsView(conn, schema, pgtraj)
    } else if (force) {
        rpostgis::dbDrop(
            conn,
            name = c(schema, sview),
            type = "materialized view",
            cascade = TRUE,
            display = TRUE
        )
        createShinyStepsView(conn, schema, pgtraj)
    }
    if (!(bview %in% relations)) {
        createShinyBurstsView(conn, schema)
    } else if (force) {
        rpostgis::dbDrop(
            conn,
            name = c(schema, bview),
            type = "materialized view",
            cascade = TRUE,
            display = TRUE
        )
        createShinyBurstsView(conn, schema)
    }
}
