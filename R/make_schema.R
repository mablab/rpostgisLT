#' Creates a traj schema in a PostGIS database.
#' 
#' @author Balázs Dukai \email{balazs.dukai@@gmail.com}
#' 
#' @param conn Connection object created with RPostgreSQL
#' @param schema String. Name of the schema that stores or will store the 
#' pgtraj data model.
#' 
#' @import rpostgis
#' @import RPostgreSQL
#' 
#' @example 
#' make_schema(conn, "traj_t4")
###############################################################################
make_schema <- function(conn, schema) {
    # Create schema
    pgSchema(conn, schema, display=FALSE, exec=TRUE)
    # Set DB search path for the schema
    query <- paste0("SET search_path TO ", schema, ",public;")
    dbSendQuery(conn, query)
    # SQL query to set up schema
    query <- paste(readLines("./inst/pgtraj_schema.sql"), collapse="\n")
    dbSendQuery(conn, query)
    # Reset DB search path to the public schema
    query <- "SET search_path TO \"$user\",public;"
    dbGetQuery(conn, query)
    print(paste("Schema", schema, "successfully created in the database."))
}