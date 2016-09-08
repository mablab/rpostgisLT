#' Imports location data from a database table into a 'traj' schema.
#' 
#' @description
#' \code{as_pgtraj} populates a \code{traj} schema from the data provided
#' in \code{relocations_table}. If the provided schema doesn't exist, it will be 
#' created. On successful data input, \code{as_pgtraj} creates three database views for
#' each pgtraj. These views are named <pgtraj_name>_parameters, 
#' <pgtraj_name>_step_geometry and <pgtraj_name>_summary and described more in
#' detail in the package vignette.
#' 
#' The time zone of the pgtraj is set to the local time zone of the user.
#' 
#' @details
#' Opening and closing connections have to be done manually by the user. 
#' However, the function checks if the provided connection is still valid. 
#' Not tested with capital letters for PostgreSQL field names.
#' 
#' @seealso Section on traj data model in the package vignette. 
#' 
#' @references \url{https://cran.r-project.org/web/packages/adehabitatLT/vignettes/adehabitatLT.pdf}
#' 
#' @author Balázs Dukai \email{balazs.dukai@@gmail.com}
#' 
#' @param conn Connection object created with RPostgreSQL
#' @param relocations_table String. Name of the schema and table that stores the relocations, e.g. c("schema","relocations")
#' @param schema String. Name of the schema that stores or will store the pgtraj data model.
#' @param pgtrajs String. Name of the pgtraj or name of the field that stores the pgtraj names.
#' @param animals String. Name of the animal or name of the field that stores the animal names.
#' @param bursts String. Name of the burst or name of the field that stores the burst names.
#' @param relocations String. Name of the field that contains the relocations 
#' in relocations_table. Relocations can be provided either as X,Y coordinates
#' or PostGIS geometry. In both cases all relocations in the 'relocations_table'
#' have to have the same projection.
#' @param timestamps String. Name of the field in relocations_table that contains the timestamps.
#' If NULL, Type I trajectory is assumed.
#' @param rids String. Name of the field in relocations_table that contains the numeric IDs of relocations.
#' @param srid Integer. Optional SRID (spatial reference ID) of (x,y) coordinates provided for relocations.
#' Ignored if relocations is a geometry type.
#' @param note String. Comment on the pgtraj. The comment is only used in
#' the database and not transferred into an ltraj.
#' 
#' @return TRUE on success
#' 
#' @examples 
#' \dontrun{
#' as_pgtraj(conn, 
#'         relocations_table = c("example_data","relocations_plus"),
#'         schema = "traj_t4",
#'         pgtrajs = "id",
#'         animals = "animal",
#'         bursts = "burst",
#'         relocations = "geom",
#'         timestamp = "time",
#'         rid = "gid")
#' }
#' 
#' \dontrun{
#' as_pgtraj(conn, 
#'         relocations_table = c("example_data","relocations_plus"),
#'         schema = "traj_t4",
#'         pgtrajs = "id",
#'         animals = "animal",
#'         bursts = "burst",
#'         relocations = c("x","y"),
#'         timestamp = "time",
#'         rid = "gid")
#' }
#' 
#' @export 
#' 
#' 
# TODO subset raw data 
# line end comment
## below line comment
### standalone
###############################################################################
as_pgtraj <- function(conn, relocations_table,  schema = "traj",
        pgtrajs = "pgtraj", animals = "animal", bursts = NULL, 
        relocations, timestamps = NULL, rids = "rid", srid = NULL,  #srid not in parameters desc.
        note = NULL) {
    ## check PostgreSQL connection and PostGIS
    if (!inherits(conn, "PostgreSQLConnection")) {
        stop("'conn' should be a PostgreSQL connection.")
    }
    if (!suppressMessages(pgPostGIS(conn))) {
        stop("PostGIS is not enabled on this database.")
    }
    # sanitize table name
    relocations_table_q <- paste(rpostgis:::dbTableNameFix(conn,relocations_table), collapse = ".")
    # sanitize column name strings used in queries
    relocations_q <- dbQuoteIdentifier(conn,relocations)
    ##### Test inputs
    # Test connection, table, field and values
    sql_query <- paste0("SELECT ", relocations_q[1], " FROM ",
            relocations_table_q," LIMIT 1;")  # should this include where is not null?
    a <- suppressWarnings(dbGetQuery(conn, sql_query)[1,1])
    if (is.null(a)) {
        print(paste("Field", relocations ,"does not contain values."))
    }
    
    # Check if the relocation geometry is projected
    if (length(relocations) == 1) {
        sql_query <- paste0("SELECT ST_SRID(", relocations_q,
        ") FROM ", relocations_table_q," LIMIT 1;")  # should this include where is not null?
        srid <- dbGetQuery(conn, sql_query)[1,1]
        if (srid == 0) {
            acr <- NA
            while(is.na(acr) | !(acr %in% "y" | acr %in% "n")) {
                acr <- readline("The projection of the data is not defined. Do you want to continue? [y/n]")
                acr <- ifelse(grepl("y|n", acr), acr, as.character(acr))
            }
            if (acr %in% "n") {
                stop("Projection is not set, returning from function.")
            }
        }
    } else {
        # if relocations are provided as X,Y coordinates
        srid <- 0
    }

    # Select proj4text from 'spatial_ref_sys'
    sch <- dbGetQuery(conn, "SELECT schemaname FROM pg_tables WHERE tablename = 'spatial_ref_sys';")[1,1]
    sql_query <- paste0("SELECT proj4text FROM ",sch,
            ".spatial_ref_sys WHERE srid = ",srid,";")
    proj4string <- dbGetQuery(conn, sql_query)[1,1]
    
    # Get user local time zone for temporary table
    time_zone <- Sys.timezone(location = TRUE)
    
    # Create traj database schema if it doesn't exist
    x <- pgTrajSchema(conn, schema)
    # If schema creation unsuccessful
    if (!isTRUE(x)) {
        stop("Traj schema couldn't be created, returning from function...")
    }
    
    ##### Insert data into temporary table
    # Begin transaction block
    invisible(dbSendQuery(conn, "BEGIN TRANSACTION;"))
    
    # Create temporary table 'zgaqtsn_temp'
    res0 <- tryCatch({
                
                pgTrajTempT(conn, schema)
                
            }, warning = function(x) {
                
                message(x)
                message(" . Rolling back transaction")
                dbRollback(conn)
                stop("Returning from function")
                
            }, error = function(x) {
                
                message(x)
                message(" . Rolling back transaction")
                dbRollback(conn)
                stop("Returning from function")
                
            })
    
    # Insert values into 'zgaqtsn_temp'
    res1 <- tryCatch({
                
                pgTrajDB2TempT(conn, schema, 
                                relocations_table, pgtrajs, animals,
                                bursts, relocations, timestamps, rids, 
                                srid, proj4string, note, time_zone)
                
            }, warning = function(x) {
                
                message("WARNING in insert into the temporary table:")
                message(x)
                message(" . Rolling back transaction")
                dbRollback(conn)
                stop("Returning from function")
                
            }, error = function(x) {
                
                message("ERROR in insert into the temporary table:")
                message(x)
                message(" . Rolling back transaction")
                dbRollback(conn)
                stop("Returning from function")
                
            })
    
    res <- c(res0, res1)
    
    ##### Insert relocations from the temporary table into the schema
    
    # Set search path in the database
    current_search_path <- dbGetQuery(conn, "SHOW search_path;")
    sql_query <- paste0("SET search_path TO ", dbQuoteIdentifier(conn,schema), ",public;")
    invisible(dbSendQuery(conn, sql_query))
    
    # Run the SQL import script to insert the data from the temporary
    # table into the traj schema
    res2 <- tryCatch({
                
                pgtraj_insert_file <- paste0(path.package("rpostgisLT"),
                    "/sql/insert_db.sql")
                sql_query <- paste(readLines(pgtraj_insert_file), collapse = "\n")
                invisible(dbSendQuery(conn, sql_query))
                TRUE
                
            }, warning = function(x) {
                
                message(x)
                message(" . Rolling back transaction")
                dbRollback(conn)
                stop("Returning from function")
                
            }, error = function(x) {
                
                message(x)
                message(". Rolling back transaction")
                dbRollback(conn)
                stop("Returning from function")
                
            })
    
    # Create views
    # FIXME remove suppressWarnings
    if (suppressWarnings(all(res))) {
        pgt <- dbGetQuery(conn,"SELECT DISTINCT pgtraj_name FROM zgaqtsn_temp;")[,1]
        for (i in pgt) {
            res3 <- tryCatch({
                    
                    pgTrajViewParams(conn, schema, pgtraj = i, srid, db = TRUE)
                    
                    # TODO create view if doesn't exist
                    pgTrajViewStepGeom(conn, schema, pgtraj = i)
                    
                    }, warning = function(x) {
                        
                        message(x)
                        message(" . Rolling back transaction")
                        dbRollback(conn)
                        stop("Returning from function")
                        
                    }, error = function(x) {
                        
                        message(x)
                        message(" . Rolling back transaction")
                        dbRollback(conn)
                        stop("Returning from function")
                        
                    })
            res <- c(res, res3)
        }
        
    }
    
    # Commit transaction and reset search path in the database
    sql_query <- paste0("SET search_path TO ", current_search_path, ";")
    invisible(dbGetQuery(conn, sql_query))
    
    if (suppressWarnings(all(res))) {
        dbCommit(conn)
        # Vacuum the tables
        suppressMessages(pgTrajVacuum(conn, schema))
        # Return TRUE
        return(all(res))
    } else {
        message("Insert faliure, rolling back transaction")
        dbRollback(conn)
    }
}