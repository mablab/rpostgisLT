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
#' @param relocation_data String. Name of the table that stores the relocations, e.g. "public.relocations"
#' @param pgtrajs String. Name of the pgtraj or name of the field that stores the pgtraj names.
#' @param animals String. Name of the animal or name of the field that stores the animal names.
#' @param bursts String. Name of the burst or name of the field that stores the burst names.
#' @param relocations String. Name of the field that contains the relocations in relocation_data.
#' @param timestamps String. Name of the field in relocation_data that contains the timestamps.
#' If NULL, Type I trajectory is assumed.
#' @param rids String. Name of the field in relocation_data that contains the numeric IDs of relocations.
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
#'         relocation_data = "example_data.relocations_plus",
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
# TODO ellaborate on transaction (t) test, probably tryCatch() with RPostgreSQL::dbRollback would work,
# TODO make sure that if any part breaks, the transaction is rolled back
# TODO after pgTrajDB2TempT success, gives a warning that WARNING:  there is already a transaction in progress,
# probably I'll need to end the tansaction with submitting a query manually, not with RPostgreSQL::dbCommit()
# TODO refactor relocation_data to 'table' or 'relocation_table', refactor relocations to ?
# line end comment
## below line comment
### standalone
###############################################################################
as_pgtraj <- function(conn, schema = "traj", relocation_data = NULL, 
        pgtrajs = "pgtraj", animals = "animal", bursts = NULL, relocations = NULL,
        timestamps = NULL, rids = "rid", epsg = NULL, db = TRUE) {
    
    ##### Insert relocations into temporary table if they are stored in the 
    # database. Otherwise proceed with an existing temporary table.
    
    if (db) {
        ##### Test input before doing anything else 
        # Test connection, table, field and values
        query <- paste0("SELECT ", relocations, " FROM ",
                relocation_data," LIMIT 1;")
        if (is.na(RPostgreSQL::dbGetQuery(conn, query)[1,1])) {
            print(paste("Field", relocations ,"does not contain values."))
        }
        
        # Check if steps has SRID
        # Optionally, reprojection in database could be included here but it would
        # make the code more complex, particulary with testing for valid SRID input
        query <- paste0("SELECT ST_SRID(", relocations,
                ") FROM ", relocation_data," LIMIT 1;")
        epsg <- RPostgreSQL::dbGetQuery(conn, query)[1,1]
        if (epsg == 0) {
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
        # If schema doesn't exists and user doesn't want to create it
        if (x == "Exit") {
            stop("Schema not created, returning from function.")
        }
        
        ##### Data input
        # Create temporary table 'relocs_temp'
        pgTrajTempT(conn, schema)
        
        # Insert values into 'relocs_temp'
        suppressMessages(pgTrajDB2TempT(conn, schema, relocation_data, pgtrajs, animals,
                bursts, relocations, timestamps, rids, epsg))
    }
    
    ##### Insert relocations from the temporary table into the schema
    
    # Begin transaction block and set search path in the database
    #invisible(RPostgreSQL::dbGetQuery(conn, "BEGIN TRANSACTION;"))
    current_search_path <- RPostgreSQL::dbGetQuery(conn, "SHOW search_path;")
    query <- paste0("SET search_path TO ", schema, ",public;")
    invisible(RPostgreSQL::dbGetQuery(conn, query))
    
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
    invisible(RPostgreSQL::dbGetQuery(conn, query))
    
    # Insert steps into the schema
    bst <- RPostgreSQL::dbGetQuery(conn,"SELECT DISTINCT b_name FROM relocs_temp;")[,1]
    invisible(RPostgreSQL::dbGetQuery(conn,"ALTER TABLE steps ADD b_name text;"))
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
        invisible(RPostgreSQL::dbGetQuery(conn, query))
        # Insert step-burst relations
        query <- paste0("INSERT INTO s_i_b_rel (s_id, b_id)
                        SELECT a.s_id, b.b_id
                        FROM steps a, bursts b
                        WHERE a.b_name = '",i , "' 
                        AND b.b_name = '", i, "'; ")
        query <- gsub(pattern = '\\s', replacement = " ", x = query)
        invisible(RPostgreSQL::dbGetQuery(conn, query))
       
        # Delete b_names from temporary column
        invisible(RPostgreSQL::dbGetQuery(conn, "UPDATE steps SET b_name = NULL;"))
    }
    # Drop temporary column
    invisible(RPostgreSQL::dbGetQuery(conn, "ALTER TABLE steps DROP COLUMN b_name;"))
    
    # Create view for step parameters
    pgt <- RPostgreSQL::dbGetQuery(conn,"SELECT DISTINCT p_name FROM relocs_temp;")[,1]
    for (i in pgt) {
        pgTrajParamsView(conn, schema, i, epsg)
    }
    
    # Commit transaction and reset search path in the database
    query <- paste0("SET search_path TO ", current_search_path, ";")
    invisible(RPostgreSQL::dbGetQuery(conn, query))
    #RPostgreSQL::dbCommit(conn)
    
    # Drop temporary table
    pgTrajDropTempT(conn, schema)
    
    return(TRUE)
}