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
            stop("Traj schema couln't be created, returning from function...")
        }
        
        ##### Data input
        # Begin transaction block
        invisible(dbSendQuery(conn, "BEGIN TRANSACTION;"))
        
        # Create temporary table 'relocs_temp'
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
        
        # Insert values into 'relocs_temp'
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
    
    # Insert relocations
    query <- paste0("INSERT INTO pgtrajs (p_name)
                    SELECT DISTINCT p_name
                    FROM relocs_temp;
                    
                    INSERT INTO animals (a_name)
                    SELECT DISTINCT a_name
                    FROM relocs_temp;
                    
                    INSERT INTO bursts (b_name, a_id)
                    SELECT DISTINCT a.b_name, b.a_id
                    FROM relocs_temp a JOIN animals b
                    ON a.a_name = b.a_name;
                    
                    INSERT INTO p_b_rel (p_id, b_id)
                    SELECT DISTINCT b.p_id, b_id
                    FROM relocs_temp a 
                    JOIN pgtrajs b
                    ON a.p_name = b.p_name
                    JOIN bursts c
                    ON a.b_name = c.b_name;")
    query <- gsub(pattern = '\\s', replacement = " ", x = query)
    
    # 'res2' is passed on to ltraj2pgtraj 
    res2 <- tryCatch({
                
                invisible(dbSendQuery(conn, query))
                TRUE
                
            }, warning = function(war) {
                
                message("WARNING in insert into 'pgtajs', 'animals', 'bursts', 'p_b_rel':")
                message(war)
                message(" . Rolling back transaction")
                dbRollback(conn)
                stop("Returning from function")
                
            }, error = function(err) {
                
                message("ERROR in insert into 'pgtajs', 'animals', 'bursts', 'p_b_rel':")
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
                
        bst <- dbGetQuery(conn,"SELECT DISTINCT b_name FROM relocs_temp;")[,1]
        invisible(dbSendQuery(conn,"ALTER TABLE steps ADD b_name text;"))
        for (i in bst) {
            query <- paste0("INSERT INTO steps (
                                r_rowname,
                                reloc1,
                                step,
                                date,
                                dt,
                                b_name
                            ) (
                                SELECT
                                CASE 
                                    WHEN a.pkey IS NOT NULL THEN a.pkey
                                    WHEN a.pkey IS NULL THEN a.r_id::text
                                END AS r_rowname,
                                CASE
                                    WHEN a.relocation IS NOT NULL AND b.relocation IS NOT NULL THEN a.relocation
                                    WHEN (a.relocation IS  NOT NULL AND b.relocation IS NULL) THEN a.relocation
                                    WHEN (a.relocation IS NULL AND b.relocation IS NOT NULL) OR 
                                         (a.relocation IS NULL AND b.relocation IS NULL) THEN NULL
                                END as reloc1,
                                CASE
                                    WHEN a.relocation IS NOT NULL AND b.relocation IS NOT NULL THEN st_makeline(a.relocation, b.relocation)
                                    WHEN (a.relocation IS  NOT NULL AND b.relocation IS NULL) THEN NULL
                                    WHEN (a.relocation IS NULL AND b.relocation IS NOT NULL) OR 
                                         (a.relocation IS NULL AND b.relocation IS NULL) THEN NULL
                                END as step,
                                    a.date,
                                    b.date - a.date AS dt,
                                    '", i, "' as b_name
                                FROM 
                                    relocs_temp AS a
                                    LEFT JOIN relocs_temp AS b 
                                        ON a.r_id + 1 = b.r_id AND
                                        a.b_name = b.b_name
                                WHERE a.b_name = '", i, "'
                                ORDER BY a.r_id
                            );")
            query <- gsub(pattern = '\\s', replacement = " ", x = query)
            invisible(dbSendQuery(conn, query))
            
            # Insert step-burst relations
            query <- paste0("INSERT INTO s_i_b_rel (s_id, b_id)
                            SELECT a.s_id, b.b_id
                            FROM steps a, bursts b
                            WHERE a.b_name = '",i , "' 
                            AND b.b_name = '", i, "'; ")
            query <- gsub(pattern = '\\s', replacement = " ", x = query)
            invisible(dbSendQuery(conn, query))
           
            # Delete b_names from temporary column
            invisible(dbSendQuery(conn, "UPDATE steps SET b_name = NULL;"))
        }
        
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
        invisible(dbGetQuery(conn, "ALTER TABLE steps DROP COLUMN b_name;"))
    }
    
    # Create view for step parameters    
    if (suppressWarnings(all(res))) {
        pgt <- dbGetQuery(conn,"SELECT DISTINCT p_name FROM relocs_temp;")[,1]
        for (i in pgt) {
            res4 <- tryCatch(
                    
                    pgTrajParamsView(conn, schema, i, srid),
                    
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
        
    }
    
    # Commit transaction and reset search path in the database
    query <- paste0("SET search_path TO ", current_search_path, ";")
    invisible(dbGetQuery(conn, query))
    
    # ltraj2pgtraj() handles transactions already, thus no need to commit here
    # if an ltraj is insterted
    if (db) {
        if (suppressWarnings(all(res))) {
            dbCommit(conn)
        } else {
            message("Insert faliure, rolling back transaction")
            dbRollback(conn)
        }
    }
    
    # Drop temporary table
    pgTrajDropTempT(conn, schema)
    
    return(all(res))
}