#' Export an ltraj object from R into a traj database schema.
#' 
#' @description 
#' \code{ltraj2pgtraj} creates a new traj schema or uses an existing one and 
#' exports an ltraj to the database. The time zone and projection information
#' stored in the ltraj is transferred to the database. Uses \code{as_pgtraj} 
#' to insert the values into the traj schema.
#' 
#' @author Balázs Dukai \email{balazs.dukai@@gmail.com}
#' 
#' @param ltraj An ltraj object.
#' @param conn Connection object created with RPostgreSQL
#' @param schema String. Name of the schema that stores or will store the pgtraj data model.
#' @param pgtraj String. Name of the new pgtraj. Defaults to the name of the 
#' object that stores the ltraj.
#' @param note String. A comment that will be stored with the pgtraj in the database.
#' @param create Logical. If no matching SRID is found, should a new SRID be created? 
#' User must have write access on spatial_ref_sys table.
#' @param new.srid Integer. Optional SRID to give to a newly created SRID. If left NULL (default),
#' the next open value of `srid` in `spatial_ref_sys` between 880000 and 890000 will be used.
#' 
#' @return TRUE on success
#' 
#' @seealso \code{\link{as_pgtraj}}
#' 
#' @examples 
#' \dontrun{ltraj2pgtraj(conn, ibex, "traj_t2")}
#' 
#' @export 
#' 
################################################################################
ltraj2pgtraj <- function(conn, ltraj, schema = "traj", pgtraj = NULL, 
        note = NULL) {
    
    ###### Format ltraj for database input
    # 'pgtraj' defaults to the name of ltraj
    if (is.null(pgtraj)) {
        pgtraj <- deparse(substitute(ltraj))
    }
    # TODO pgtraj can only contain DB table-name-proof characters, include in test_input()

    # Set projection
    srs <- attr(ltraj, "proj4string")
    if (is.null(srs)) {
        srid <- 0
    } else {
        srid <- pgSRID(conn = conn, crs = srs, create.srid = TRUE, 
                        new.srid = NULL)
        srs <- srs@projargs
    }
    # Convert ltraj to data frame
    dframe <- ld_opt(ltraj)
    # Get time zone, srs, proj4string, note, pgtraj
    dframe$.time_zone <- attr(ltraj[[1]]$date, "tzone")
    dframe$.srid <- srid
    dframe$.proj4string <- srs
    dframe$.pgtraj <- pgtraj
    dframe$.note <- note
    # Format date to include time zone that Postgres recognizes
    dframe$date <- sapply(dframe$date, function(x) strftime(x,
                        format = "%Y-%m-%d %H:%M:%S", tz = "", usetz = TRUE))
    # Parameters to exclude on input
    params <- c("dist", "abs.angle")
    
    ###### Check and create a pgtraj schema
    # pgTrajSchema() has its own transaction control
    x <- pgTrajSchema(conn, schema)
    # If schema creation unsuccessful
    if (!isTRUE(x)) {
        stop("Traj schema couldn't be created, returning from function.")
    }
    
    ###### Begin transaction block and input to postgres
    invisible(dbSendQuery(conn, "BEGIN TRANSACTION;"))
    
    # Set database search path
    current_search_path <- dbGetQuery(conn, "SHOW search_path;")
    sql_query <- paste0("SET search_path TO ", schema, ",public;")
    invisible(dbGetQuery(conn, sql_query))
    
    # Import data frame into a temporary table
    res <- tryCatch({
                
                invisible(dbWriteTable(conn, name="zgaqtsn_temp", 
                                value=dframe[, -which(names(dframe) %in% params)],
                                row.names=FALSE))
                TRUE
                
            }, warning = function(x) {
                
                message(x)
                message(". Rolling back transaction")
                dbRollback(conn)
                stop("Returning from function")
                
            }, error = function(x) {
                
                message(x)
                message(". Rolling back transaction")
                dbRollback(conn)
                stop("Returning from function")
                
            })
    # Run the SQL import script to insert the data from the temporary
    # table into the traj schema
    res2 <- tryCatch({
                
                pgtraj_insert_file <- paste0(path.package("rpostgisLT"),
                    "/sql/insert_ltraj.sql")
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
    
    res <- c(res, res2)
    
    # Drop temporary table
    invisible(dbSendQuery(conn, "DROP TABLE zgaqtsn_temp;"))

###### TODO Create parameter and geometry views

    tryCatch({
        if(all(res)) {
            # Restore database search path
            sql_query <- paste0("SET search_path TO ", current_search_path, ";")
            invisible(dbSendQuery(conn, sql_query))

            dbCommit(conn)

            message(paste0("The ltraj '", pgtraj,
                            "' successfully inserted into the database schema '",
                            schema,"'."))

            return(TRUE)
        } else {
            dbRollback(conn)
            stop("Ltraj insert failed")
        }
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

#    # Insert CRS, note and time zone on the pgtraj
#    if (all(res)) {
#        # Restore database search path
#        sql_query <- paste0("SET search_path TO ", current_search_path, ";")
#        invisible(dbSendQuery(conn, sql_query))
#        
#        dbCommit(conn)
#        
#        message(paste0("The ltraj '", pgtraj, "' successfully inserted into the database schema '", schema,"'."))
#        
#        return(TRUE)
#    } else {
#        dbRollback(conn)
#        stop("Ltraj insert failed")
#    }
#    
}
