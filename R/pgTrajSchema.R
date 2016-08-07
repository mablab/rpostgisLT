##' Checks if the provided schema exists in the database, and creates
##' if it doesn't.
##'
##' Creates a 'traj' schema in the database by calling a SQL script 
##' from (./inst/sql/). The schema name defaults to 'traj'. If a schema with 
##' the provided name already exists in the database, it checks if it contains
##' all the required tables. The function does not attempt to repair the schema
##' if not all traj tables are present (e.g. because some were manually deleted).
##' In this case, a new traj schema needs to be created.
##'
##' @details
##' The function has it's standalone transaction control.
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
    # schema if it doesn't exist
    
    ## Check if PostGIS is installed
    suppressMessages(pgPostGIS(conn))
    
    ## Begin transaction block
    invisible(dbGetQuery(conn, "BEGIN TRANSACTION;"))
    
    ## Check and/or create schema
    x <- dbSchema(conn, name, display = FALSE, exec = TRUE)
    
    if (x) {
        # Is the traj schema in the DB or just created and empty
        query <- paste0("SELECT tablename FROM pg_tables WHERE schemaname='",name,"';")
        dbtables <- dbGetQuery(conn, query, stringsAsFactors = FALSE)
        dbtables <- dbtables$tablename
        traj_tables <- c("animal_burst", "pgtraj", "step", "infoloc", 
                "s_i_b_rel", "relocation")
        
        if (length(dbtables) == 0) {
            ## In case of empty schema
            ## Set DB search path for the schema
            current_search_path <- dbGetQuery(conn, "SHOW search_path;")
            query <- paste0("SET search_path TO ", name, ",public;")
            invisible(dbGetQuery(conn, query))
            
            ## SQL query to set up schema
            pgtraj_schema_file <- paste0(path.package("rpostgisLT"),
                    "/sql/traj_schema.sql")
            query <- paste(readLines(pgtraj_schema_file), collapse = "\n")
            invisible(dbGetQuery(conn, query))
            
            ## Reset DB search path to the public schema
            query <- paste0("SET search_path TO ", current_search_path, ";")
            invisible(dbGetQuery(conn, query))
            
            ## Commit transaction block
            invisible(dbCommit(conn))
            
            message(paste0("The traj schema '", name, "' created in the database."))
            
            return(TRUE)
        } else if (all(traj_tables %in% dbtables)) {
            # All required tables are present in the schema
            invisible(dbCommit(conn))
            
            message(paste0 ("The traj schema '", name, "' already exists in the database."))
            
            return(TRUE)
        } else {
            invisible(dbRollback(conn))
            
            stop(paste0("The traj schema '",name,"' appears to be incomplete. Please create a new schema."))
        }
    } else {
        invisible(dbRollback(conn))
        stop(paste0("Schema '",name,"' couldn't be created."))
    }
}
