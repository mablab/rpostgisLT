#' Converts relocation data into the pgtraj data model.
#' 
#' @description
#' This is the core function of the \code{rpostgisLT} package and it is also 
#' used by \code{\link{ltraj2pgtraj}} to import trajectory data into a pgtraj 
#' data model. \code{as_pgtraj} copies the trajectory data which is stored in 
#' a database to a traj schema. If the provided schema doesn't exist, it is 
#' created on demand. On successful data input, \code{as_pgtraj} creates a view for
#' each pgtraj, with the views named as <pgtraj>_params. The view contains the same step parameters 
#' as an ltraj object (e.g. R2n, rel.angle, dt...). If the input geometries
#' are projected, their projection is used to create the steps in the schema, 
#' otherwise either no projection is used or the fuction exits.
#' 
#' @details
#' Opening and closing connections have to be done manually by the user. 
#' However, the function checks if the provided connection is still valid. 
#' Not tested with capital letters for PostgreSQL field names, but it probably won't 
#' work. Its a bad practice anyway to force uppercase in PostgreSQL so use lowercase.
#' 
#' @seealso Section on pgtraj data model in the package vignette. 
#' 
#' @references \url{https://cran.r-project.org/web/packages/adehabitatLT/vignettes/adehabitatLT.pdf}
#' 
#' @author Balázs Dukai \email{balazs.dukai@@gmail.com}
#' 
#' @param conn Connection object created with RPostgreSQL
#' @param schema String. Name of the schema that stores or will store the pgtraj data model.
#' @param relocations_table String. Name of the table that stores the relocations, e.g. "public.relocations"
#' @param pgtrajs String. Name of the pgtraj or name of the field that stores the pgtraj names.
#' @param animals String. Name of the animal or name of the field that stores the animal names.
#' @param bursts String. Name of the burst or name of the field that stores the burst names.
#' @param relocations String. Name of the field that contains the relocations in relocations_table.
#' @param timestamps String. Name of the field in relocations_table that contains the timestamps.
#' If NULL, Type I trajectory is assumed.
#' @param rids String. Name of the field in relocations_table that contains the numeric IDs of relocations.
#' @param db Boolean. If TRUE, the relocations are stored in a database table, 
#' if FALSE relocations are stored in an R object. It is meant to be used by other functions internally. 
#' If you want to import an ltraj from R, use ltraj2pgtraj().
#' 
#' @return TRUE on success
#' 
#' @examples 
#' \dontrun{
#' as_pgtraj(conn, 
#'         schema = "traj_t4",
#'         relocations_table = "example_data.relocations_plus",
#'         pgtrajs = "id",
#'         animals = "animal",
#'         bursts = "burst",
#'         relocations = "geom",
#'         timestamp = "time",
#'         rid = "gid")
#' }
#' 
#' @export 
#' 
#' 
# TODO test capital letters in field names
# TODO subset raw data 
# line end comment
## below line comment
### standalone
###############################################################################
as_pgtraj <- function(conn, schema = "traj", relocations_table = NULL, 
        pgtrajs = "pgtraj", animals = "animal", bursts = NULL, relocations = NULL,
        timestamps = NULL, rids = "rid", srid = NULL, db = TRUE) {
    
    ##### Insert relocations into temporary table if they are stored in the 
    # database. Otherwise proceed with an existing temporary table.
    
    if (db) {
        
        #TODO include comment
        
        ##### Test input before doing anything else 
        # Test connection, table, field and values
        query <- paste0("SELECT ", relocations, " FROM ",
                relocations_table," LIMIT 1;")
        a <- suppressWarnings(dbGetQuery(conn, query)[1,1])
        if (is.null(a)) {
            print(paste("Field", relocations ,"does not contain values."))
        }
        
        # Check if steps has SRID
        # Optionally, reprojection in database could be included here but it would
        # make the code more complex, particulary with testing for valid SRID input
        query <- paste0("SELECT ST_SRID(", relocations,
                ") FROM ", relocations_table," LIMIT 1;")
        srid <- dbGetQuery(conn, query)[1,1]
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
        
        # Create traj database schema if it doesn't exist
        x <- pgTrajSchema(conn, schema)
        # If schema creation unsuccessful
        if (!isTRUE(x)) {
            stop("Traj schema couldn't be created, returning from function...")
        }
        
        ##### Data input
        # Begin transaction block
        invisible(dbSendQuery(conn, "BEGIN TRANSACTION;"))
        
        # Create temporary table 'qqbqahfsbrpq_temp'
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
        
        # Insert values into 'qqbqahfsbrpq_temp'
        res1 <- tryCatch({
                    
                    pgTrajDB2TempT(conn, schema, 
                                    relocations_table, pgtrajs, animals,
                                    bursts, relocations, timestamps, rids, 
                                    srid)
                    
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
        
    }
    
    ##### Insert relocations from the temporary table into the schema
    
    # Set search path in the database
    current_search_path <- dbGetQuery(conn, "SHOW search_path;")
    query <- paste0("SET search_path TO ", schema, ",public;")
    invisible(dbSendQuery(conn, query))
    
    # Add temporary fields
    invisible(dbSendQuery(conn,"ALTER TABLE relocation ADD COLUMN burst_name text, ADD COLUMN pgtraj_name text;"))
    invisible(dbSendQuery(conn,"ALTER TABLE step ADD COLUMN burst_name text, ADD COLUMN pgtraj_name text;"))
    
    # Insert relocations
    query <- paste0("
                    INSERT INTO pgtraj (pgtraj_name)
                    SELECT DISTINCT pgtraj_name
                    FROM qqbqahfsbrpq_temp;
                    
                    INSERT INTO animal_burst (burst_name, animal_name, pgtraj_id)
                    SELECT DISTINCT a.burst_name, a.animal_name, b.id
                    FROM qqbqahfsbrpq_temp a JOIN pgtraj b
                    ON a.pgtraj_name = b.pgtraj_name;
                    
                    INSERT INTO relocation (geom, relocation_time, r_rowname, burst_name, pgtraj_name)
                    SELECT geom, relocation_time, id, burst_name, pgtraj_name
                    FROM qqbqahfsbrpq_temp;
                    ")
    query <- gsub(pattern = '\\s', replacement = " ", x = query)
    
    # 'res2' is passed on to ltraj2pgtraj 
    res2 <- tryCatch({
                
                invisible(dbSendQuery(conn, query))
                TRUE
                
            }, warning = function(war) {
                
                message("WARNING in insert into 'pgtaj', 'animal_burst', 'relocation':")
                message(war)
                message(" . Rolling back transaction")
                dbRollback(conn)
                stop("Returning from function")
                
            }, error = function(err) {
                
                message("ERROR in insert into 'pgtaj', 'animal_burst', 'relocation':")
                message(err)
                message(" . Rolling back transaction")
                dbRollback(conn)
                stop("Returning from function")
                
            })
    
    # Get res right when coming from ltraj2pgtraj
    if (db) {
        res <- c(res, res2)
    } else {
        res <- res2
    }
    
    # Insert steps into the schema
    res3 <- tryCatch({
            
            query <- paste0("
                            INSERT INTO step (
                                relocation_id_1, 
                                relocation_id_2, dt, 
                                burst_name, 
                                pgtraj_name)
                            SELECT 
                                a.id AS relocation_id_1,
                                b.id AS relocation_id_2,
                                b.relocation_time - a.relocation_time AS dt,
                                a.burst_name,
                                a.pgtraj_name
                            FROM relocation a
                            LEFT JOIN LATERAL (SELECT *
                                               FROM relocation c
                                               WHERE a.relocation_time < c.relocation_time
                                               AND a.burst_name = c.burst_name
                                               LIMIT 1
                                              ) AS b 
                            ON TRUE;
                            ")
            query <- gsub(pattern = '\\s', replacement = " ", x = query)
            invisible(dbGetQuery(conn, query))
            
            query <- paste0("
                            INSERT INTO s_i_b_rel (step_id, animal_burst_id)
                            WITH pg AS (
                                SELECT a.id, a.burst_name, a.animal_name, b.pgtraj_name
                                FROM animal_burst a
                                JOIN pgtraj b ON a.pgtraj_id = b.id
                            )
                            SELECT step.id, pg.id
                            FROM step, pg
                            WHERE (step.burst_name = pg.burst_name) 
                            	AND (step.pgtraj_name = pg.pgtraj_name);
                            ")
            query <- gsub(pattern = '\\s', replacement = " ", x = query)
            invisible(dbGetQuery(conn, query))
            
        }, warning = function(war) {
            
            message("WARNING in insert into 'steps':")
            message(war)
            message(" . Rolling back transaction")
            dbRollback(conn)
            stop("Returning from function")
            
        }, error = function(err) {
            
            message("ERROR in insert into 'steps':")
            message(err)
            message(" . Rolling back transaction")
            dbRollback(conn)
            stop("Returning from function")
            
        })
        
    res <- c(res, res3)
    
    # Drop temporary column
    if (suppressWarnings(all(res))) {
        invisible(dbGetQuery(conn, "ALTER TABLE relocation DROP COLUMN burst_name, DROP COLUMN pgtraj_name;"))
        invisible(dbGetQuery(conn, "ALTER TABLE step DROP COLUMN burst_name, DROP COLUMN pgtraj_name;"))
    } else {
        message("ERROR. Rolling back transaction.")
        dbRollback(conn)
    }
    
    # Create views
    if (suppressWarnings(all(res))) {
        pgt <- dbGetQuery(conn,"SELECT DISTINCT pgtraj_name FROM qqbqahfsbrpq_temp;")[,1]
        for (i in pgt) {
            res4 <- tryCatch(
                    
                    pgTrajViewParams(conn, schema, i, srid),
                    
                    warning = function(x) {
                        
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
            res <- c(res, res4)
        }
        
        pgTrajViewStepGeom(conn, schema)
    }
    
    # Drop temporary table
    invisible(dbGetQuery(conn, "DROP TABLE qqbqahfsbrpq_temp;"))
    
    # Commit transaction and reset search path in the database
    query <- paste0("SET search_path TO ", current_search_path, ";")
    invisible(dbGetQuery(conn, query))
    
    # ltraj2pgtraj() handles transactions already, thus no need to commit here
    # if an ltraj is insterted
    if (db) {
        if (suppressWarnings(all(res))) {
            dbCommit(conn)
            return(all(res))
        } else {
            message("Insert faliure, rolling back transaction")
            dbRollback(conn)
        }
    }
    
    return(all(res))
}