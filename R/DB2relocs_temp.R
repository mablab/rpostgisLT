#' Insert relocations from a source table into the table 'relocs_temp'. 
#' 
#' If relocations are given as X,Y coordinates, they are converted into a POINT 
#' geometry in PostGIS.
#' 
#' @author Balázs Dukai
#' 
#' @param conn Connection object created with RPostgreSQL
#' @param schema String. Name of the schema that stores or will store the pgtraj data model.
#' @param relocation_table String. Name of the table that stores the relocation_geom, e.g. "public.relocations"
#' @param pgtrajs String. Name of the pgtraj or name of the field that stores the pgtraj names.
#' @param animals String. Name of the animal or name of the field that stores the animal names.
#' @param bursts String. Name of the burst or name of the field that stores the burst names.
#' @param timestamps String. Name of the field in relocation_table that contains the timestamps.
#' @param rids String. Name of the field in relocation_table that contains the numeric IDs of relocations.
#' @param relocation_geom Vector of string(s). Name of the field(s) that contains 
#' the relocations in relocation_table. If relocations are stored as pairs of (X,Y) or 
#' (long, lat) coorindates, the coordinates should be separeted in two fields 
#' and referenced accordingly.
#' @param epsg Numeric. EPSG code of the CRS of 'relocations'.
#' 
#' 
#' 
###############################################################################
DB2relocs_temp <- function(conn, schema, relocation_table, pgtrajs, animals,
        bursts = NA, relocation_geom, timestamps, rids, epsg) {
    
    # Test for correct inputs
    test_input(pgtrajs, animals, relocation_geom, bursts)
    
    # Set DB search path for the schema
    query <- paste0("SET search_path TO ", schema, ",public;")
    invisible(dbGetQuery(conn, query))
    
    # Table name is separated from schema declaration
    rd_split <- unlist(strsplit(relocation_table, "[.]"))
    
    # Begin transaction block (hence make_relocs_temp() operation is "all or nothing")
    invisible(dbGetQuery(conn, "BEGIN TRANSACTION;"))
    
    # Populate 'relocs_temp'----------------------------------------------------
    # Insert relocation_geom if trajectory Type I
    if (is.na(timestamps)) {
        # Relocations provided as point geometry
        if (length(relocation_geom) == 1) {
            query <- paste0("INSERT INTO relocs_temp (r_id, relocation)
                            SELECT ",rids,",",relocation_geom,"::geometry
                            FROM ",relocation_table,"
                            ORDER BY ",rids,";")
            query <- gsub(pattern = '\\s', replacement = " ", x = query)
            t <- c(t, dbGetQuery(conn, query))
        } else if (length(relocation_geom) == 2) {
            # Relocations provided as a coordinate pair
            x <- relocation_geom[1]
            y <- relocation_geom[2]
            query <- paste0("INSERT INTO relocs_temp (r_id, relocation)
                            SELECT ",rids,", ST_SetSRID(ST_MakePoint(",x,", ",y,"), ",epsg,")
                            FROM ",relocation_table,"
                            ORDER BY ",rids,";")
            query <- gsub(pattern = '\\s', replacement = " ", x = query)
            invisible(dbGetQuery(conn, query))
        }
    # If trajectory Type II
    } else {
        if (length(relocation_geom) == 1) {
            # Relocations provided as point geometry
            query <- paste0("INSERT INTO relocs_temp (r_id, relocation, date)
                            SELECT ",rids,",",relocation_geom,"::geometry, ",timestamps,"
                            FROM ",relocation_table,"
                            ORDER BY ",timestamps,";")
            query <- gsub(pattern = '\\s', replacement = " ", x = query)
            invisible(dbGetQuery(conn, query))
        } else if (length(relocation_geom) == 2) {
            # relocation_geom provided as a coordinate pair
            x <- relocation_geom[1]
            y <- relocation_geom[2]
            query <- paste0("INSERT INTO relocs_temp (r_id, relocation, date)
                            SELECT ",rids,", ST_SetSRID(ST_MakePoint(",x,", ",y,"), ",epsg,"), ",timestamps,"
                            FROM ",relocation_table,"
                            ORDER BY ",timestamps,";")
            query <- gsub(pattern = '\\s', replacement = " ", x = query)
            invisible(dbGetQuery(conn, query))
        }
    }
    
    fields <- dbListFields(conn, rd_split)
    # Insert pgtraj
    if (pgtrajs %in% fields) {
        # use the field values for pgtraj
        query <- paste0("UPDATE relocs_temp
                        SET p_name = a.",pgtrajs,"
                        FROM (
                        SELECT ",rids,", ",pgtrajs,"
                        FROM ",relocation_table,"
                        ) a
                        WHERE r_id = a.",rids,";")
        query <- gsub(pattern = '\\s', replacement = " ", x = query)
        invisible(dbGetQuery(conn, query))
    } else {
        # Use the string
        query <- paste0("UPDATE relocs_temp SET p_name = '", pgtrajs, "';")
        invisible(dbGetQuery(conn, query))
    }
    
    # Insert animal
    if (animals %in% fields) {
        # Use the field values for animal
        query <- paste0("UPDATE relocs_temp
                        SET a_name = a.",animals,"
                        FROM (
                        SELECT ",rids,", ",animals,"
                        FROM ",relocation_table,"
                        ) a
                        WHERE r_id = a.",rids,";")
        query <- gsub(pattern = '\\s', replacement = " ", x = query)
        invisible(dbGetQuery(conn, query))
    } else {
        # Use the string
        query <- paste("UPDATE relocs_temp SET a_name = '", animals, "';")
        invisible(dbGetQuery(conn, query))
    }
    
    # Insert burst
    if (bursts %in% fields) {
        # Use the field values for bursts
        query <- paste0("UPDATE relocs_temp
                        SET b_name = a.",bursts,"
                        FROM (
                        SELECT ",rids,", ",bursts,"
                        FROM ",relocation_table,"
                        ) a
                        WHERE r_id = a.",rids,";")
        query <- gsub(pattern = '\\s', replacement = " ", x = query)
        invisible(dbGetQuery(conn, query))
    } else if (is.na(bursts) & length(animals) > 1){
        # Use animal name as default burst name
        query <- paste0("UPDATE relocs_temp
                        SET b_name = a.",animals,"
                        FROM (
                        SELECT ",rids,", ",animals,"
                        FROM ",relocation_table,"
                        ) a
                        WHERE r_id = a.",rids,";")
        query <- gsub(pattern = '\\s', replacement = " ", x = query)
        invisible(dbGetQuery(conn, query))
    } else if (is.na(bursts) & length(animals) == 1) {
        query <- paste0("UPDATE relocs_temp SET b_name = '",animals,"';")
        invisible(dbGetQuery(conn, query))
    } else {
        # Use the string
        query <- paste("UPDATE relocs_temp SET b_name = '", bursts, "';")
        invisible(dbGetQuery(conn, query))
    }
    
    # Commit transaction
    dbCommit(conn)
    message("Values were successfully inserted into 'relocs_temp'.")
    
    # Reset DB search path to the public schema
    query <- "SET search_path TO \"$user\",public;"
    invisible(dbGetQuery(conn, query))
}


