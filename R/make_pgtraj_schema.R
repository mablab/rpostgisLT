##' Checks if the provided schema exists in the database, and creates
##' if it doesn't.
##'
##' [provide details on how the function operates]
##' @title Check 'traj' schema.
##' @param conn Connection object created with RPostgreSQL.
##' @param schema Chaaracter string. Name of the schema that stores or
##'     will store the pgtraj data model.
##' @return [what does it return?]
##' @author Balázs Dukai \email{balazs.dukai@@gmail.com}
##' @export
##' @example [provide example]
make_pgtraj_schema <- function(conn, name = "traj") {
    # Create traj database schema if it doesn't exist
    # TODO Check if also all necessary tables exist
    query <- "SELECT nspname FROM pg_catalog.pg_namespace;"
    schemas <- dbGetQuery(conn, query)[,1]
    if (schema %in% schemas) {
        message(paste("Schema", schema ,
                        "already exists in the database."))
        return(FALSE)
    } else {
        # Create traj schema
        acr <- NA
        while(is.na(acr) | !(acr %in% "y" | acr %in% "n")) {
            acr <- readline(paste("Schema", schema,
                            "does not exist in the database. Do you want to create it? [y/n]"))
            acr <- ifelse(grepl("y|n", acr), acr, as.character(acr))
        }
        if (acr %in% "n") {
            return("Exit")
        } else {
            # Begin transaction block
            invisible(dbGetQuery(conn, "BEGIN TRANSACTION;"))
            # Create schema
            pgSchema(conn, schema, display=FALSE, exec=TRUE)
            # Set DB search path for the schema
            query <- paste0("SET search_path TO ", schema, ",public;")
            invisible(dbGetQuery(conn, query))
            # SQL query to set up schema
            if (file.exists("./inst/pgtraj_schema.sql")) {
                query <- paste(readLines("./inst/pgtraj_schema.sql"), collapse="\n")
                invisible(dbGetQuery(conn, query))
            } else {
                dbRollback(conn)
                stop("Cannot find 'pgtraj_schema.sql'")
            }
            # Reset DB search path to the public schema
            query <- "SET search_path TO \"$user\",public;"
            invisible(dbGetQuery(conn, query))

            # Commit transaction block
            invisible(dbCommit(conn))

            message(paste("Schema", schema, "successfully created in the database."))

            return(TRUE)
        }
    }
}

