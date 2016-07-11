#' Function converts relocation data into the pgtraj data model.
#' 
#' Opening and closing connections have to be done manually by the user. 
#' However, the function checks if the provided connection is still valid.
#'
#' @author Balázs Dukai
#' 
#' @param conn Connection object created with RPostgreSQL
#' @param schema String. Name of the schema that stores or will store the pgtraj data model.
#' @param relocation_data String. Name of the table that stores the relocations, e.g. "public.relocations"
#' @param pgtrajs String. Name of the pgtraj or name of the field that stores the pgtraj names.
#' @param animals String. Name of the animal or name of the field that stores the animal names.
#' @param bursts String. Name of the burst or name of the field that stores the burst names.
#' @param relocations String. Name of the field that contains the relocations in relocation_data.
#' @param timestamps String. Name of the field in relocation_data that contains the timestamps.
#' @param rids String. Name of the field in relocation_data that contains the numeric IDs of relocations.
#' @param db Boolean. If TRUE, the relocations are stored in a database table, 
#' if FALSE relocations are stored in an R object. It is meant to be used by other functions internally. 
#' If you want to import an ltraj from R, use ltraj2pgtraj().
#' 
#' Not tested with capital letters for PostgreSQL field names, but it probably won't 
#' work. Its a bad practice anyway to force uppercase in PostgreSQL so use lowercase.
#' 
#' @export 
#' 
# TODO test capital letters in field names
# TODO subset raw data 
# TODO ellaborate on transaction (t) test, probably tryCatch() with dbRollback would work
# line end comment
## below line comment
### standalone


###############################################################################
as_pgtraj <- function(conn, schema = "pgtraj", relocation_data = NULL, 
        pgtrajs = "pgtraj", animals = "animal", bursts = NULL, relocations = NULL,
        timestamps = NULL, rids = "rid", db = TRUE) {
    
    ##### Insert relocations into temporary table if they are stored in the 
    # database. Otherwise proceed with an existing temporary table.
    
    if (db) {
        ##### Test input before doing anything else 
        # Test connection, table, field and values
        query <- paste0("SELECT ", relocations, " FROM ",
                relocation_data," LIMIT 1;")
        if (is.na(dbGetQuery(conn, query)[1,1])) {
            print(paste("Field", relocations ,"does not contain values."))
        }
        
        # Check if steps has SRID
        # Optionally, reprojection in database could be included here but it would
        # make the code more complex, particulary with testing for valid SRID input
        query <- paste0("SELECT ST_SRID(", relocations,
                ") FROM ", relocation_data," LIMIT 1;")
        epsg <- dbGetQuery(conn, query)[1,1]
        if (epsg == 0) {
            acr <- NA
            while(is.na(acr) | !(acr %in% "y" | acr %in% "n")) {
                acr <- readline("The projection of the data is not defined. Do you want to continue? [y/n]")
                acr <- ifelse(grepl("y|n", acr), acr, as.character(acr))
            }
            if (acr %in% "n") {
                stop("Returning from function...")
            }
        }
        
        # Create traj database schema if it doesn't exist
        x <- make_pgtraj_schema(conn, schema)
        # If schema doesn't exists and user doesn't want to create it
        if (x == 0) {
            stop("Returning from function...")
        }
        
        ##### Data input
        # Create temporary table 'relocs_temp'
        make_relocs_temp(conn, schema)
        
        # Insert values into 'relocs_temp'
        DB2relocs_temp(conn, schema, relocation_data, pgtrajs, animals,
                bursts, relocations, timestamps, rids, epsg)
    }
    
    ##### Insert relocations from the temporary table into the schema
    
    # Begin transaction block and set search path in the database
    invisible(dbGetQuery(conn, "BEGIN TRANSACTION;"))
    query <- paste0("SET search_path TO ", schema, ",public;")
    invisible(dbGetQuery(conn, query))
    
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
    invisible(dbGetQuery(conn, query))
    
    # Insert steps into the schema
    bst <- dbGetQuery(conn,"SELECT DISTINCT b_name FROM relocs_temp;")[,1]
    invisible(dbGetQuery(conn,"ALTER TABLE steps ADD b_name text;"))
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
                                '",i,"'
                            FROM 
                                relocs_temp AS a
                                INNER JOIN relocs_temp AS b 
                                    ON a.r_id + 1 = b.r_id AND
                                    a.b_name = b.b_name
                            WHERE a.b_name = '", i, "'
                            ORDER BY a.r_id
                        );")
        query <- gsub(pattern = '\\s', replacement = " ", x = query)
        invisible(dbGetQuery(conn, query))
        # Insert step-burst relations
        query <- paste0("INSERT INTO s_i_b_rel (s_id, b_id)
                        SELECT a.s_id, b.b_id
                        FROM steps a, bursts b
                        WHERE a.b_name = '",i , "' 
                        AND b.b_name = '", i, "'; "
        )
        query <- gsub(pattern = '\\s', replacement = " ", x = query)
        invisible(dbGetQuery(conn, query))
       
        # Delete b_names from temporary column
        invisible(dbGetQuery(conn, "UPDATE steps SET b_name = NULL;"))
    }
    # Drop temporary column
    invisible(dbGetQuery(conn, "ALTER TABLE steps DROP COLUMN b_name;"))
    
    # Commit transaction and reset search path in the database
    query <- "SET search_path TO \"$user\",public;"
    invisible(dbGetQuery(conn, query))
    dbCommit(conn)
    
    # Drop temporary table
    drop_relocs_temp(conn, schema)
    
    return(TRUE)
}