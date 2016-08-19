#' Insert relocations from a source table into the table 'qqbqahfsbrpq_temp'. 
#' 
#' If relocations are given as X,Y coordinates, they are converted into a POINT 
#' geometry in PostGIS.
#' 
#' @author Balázs Dukai
#' 
#' @param conn Connection object created with RPostgreSQL
#' @param schema String. Name of the schema that stores or will store the pgtraj data model.
#' @param relocations_table String. Name of the table that stores the relocations, e.g. "public.relocations"
#' @param pgtrajs String. Name of the pgtraj or name of the field that stores the pgtraj names.
#' @param animals String. Name of the animal or name of the field that stores the animal names.
#' @param bursts String. Name of the burst or name of the field that stores the burst names.
#' @param timestamps String. Name of the field in relocations_table that contains the timestamps.
#' @param rids String. Name of the field in relocations_table that contains the numeric IDs of relocations.
#' @param relocations Vector of string(s). Name of the field(s) that contains 
#' the relocations in relocations_table. If relocations are stored as pairs of (X,Y) or 
#' (long, lat) coorindates, the coordinates should be separeted in two fields 
#' and referenced accordingly.
#' @param srid Numeric. The PostGIS SRID of the CRS of 'relocations'.
#' 
#' 
#' 
###############################################################################
pgTrajDB2TempT <- function(conn, schema, relocations_table, pgtrajs, animals,
        bursts = NULL, relocations, timestamps, rids, srid) {
    
    # Test for correct inputs
    test_input(pgtrajs, animals, relocations, bursts)
    
    # Set DB search path for the schema
    current_search_path <- dbGetQuery(conn, "SHOW search_path;")
    sql_query <- paste0("SET search_path TO ", schema, ",public;")
    invisible(dbSendQuery(conn, sql_query))
    
    # Table name is separated from schema declaration
    rd_split <- unlist(strsplit(relocations_table, "[.]"))
    
    # Populate 'qqbqahfsbrpq_temp'----------------------------------------------------
    # Insert relocations if trajectory Type I
    if (is.null(timestamps)) {
        # Relocations provided as point geometry
        if (length(relocations) == 1) {
            
            sql_query <- paste0("INSERT INTO qqbqahfsbrpq_temp (id, geom)
                            SELECT ",rids,",",relocations,"::geometry
                            FROM ",relocations_table,"
                            ORDER BY ",rids,";")
            sql_query <- gsub(pattern = '\\s', replacement = " ", x = sql_query)
            t <- c(t, dbSendQuery(conn, sql_query))
            
        } else if (length(relocations) == 2) {
            
            # Relocations provided as a coordinate pair
            x <- relocations[1]
            y <- relocations[2]
            sql_query <- paste0("INSERT INTO qqbqahfsbrpq_temp (id, geom)
                            SELECT ",rids,", ST_SetSRID(ST_MakePoint(",x,", ",y,"), ",srid,")
                            FROM ",relocations_table,"
                            ORDER BY ",rids,";")
            sql_query <- gsub(pattern = '\\s', replacement = " ", x = sql_query)
            invisible(dbSendQuery(conn, sql_query))
            
        }
    # If trajectory Type II
    } else {
        
        if (length(relocations) == 1) {
            
            # Relocations provided as point geometry
            sql_query <- paste0("INSERT INTO qqbqahfsbrpq_temp (id, geom, relocation_time)
                            SELECT ",rids,",",relocations,"::geometry, ",timestamps,"
                            FROM ",relocations_table,"
                            ORDER BY ",timestamps,";")
            sql_query <- gsub(pattern = '\\s', replacement = " ", x = sql_query)
            invisible(dbSendQuery(conn, sql_query))
            
        } else if (length(relocations) == 2) {
            
            # relocations provided as a coordinate pair
            x <- relocations[1]
            y <- relocations[2]
            sql_query <- paste0("INSERT INTO qqbqahfsbrpq_temp (id, geom, relocation_time)
                            SELECT ",rids,", ST_SetSRID(ST_MakePoint(",x,", ",y,"), ",srid,"), ",timestamps,"
                            FROM ",relocations_table,"
                            ORDER BY ",timestamps,";")
            sql_query <- gsub(pattern = '\\s', replacement = " ", x = sql_query)
            invisible(dbSendQuery(conn, sql_query))
        }
        
    }
    
    fields <- dbListFields(conn, rd_split)
    # Insert pgtraj
    if (pgtrajs %in% fields) {
        
        # use the field values for pgtraj
        sql_query <- paste0("UPDATE qqbqahfsbrpq_temp
                        SET pgtraj_name = a.",pgtrajs,"
                        FROM (
                        SELECT ",rids,", ",pgtrajs,"
                        FROM ",relocations_table,"
                        ) a
                        WHERE qqbqahfsbrpq_temp.id = a.",rids,";")
        sql_query <- gsub(pattern = '\\s', replacement = " ", x = sql_query)
        invisible(dbSendQuery(conn, sql_query))
        
    } else {
        
        # Use the string
        sql_query <- paste0("UPDATE qqbqahfsbrpq_temp SET pgtraj_name = '", pgtrajs, "';")
        invisible(dbSendQuery(conn, sql_query))
        
    }
    
    # Insert animal
    if (animals %in% fields) {
        
        # Use the field values for animal
        sql_query <- paste0("UPDATE qqbqahfsbrpq_temp
                        SET animal_name = a.",animals,"
                        FROM (
                        SELECT ",rids,", ",animals,"
                        FROM ",relocations_table,"
                        ) a
                        WHERE qqbqahfsbrpq_temp.id = a.",rids,";")
        sql_query <- gsub(pattern = '\\s', replacement = " ", x = sql_query)
        invisible(dbSendQuery(conn, sql_query))
        
    } else {
        
        # Use the string
        sql_query <- paste("UPDATE qqbqahfsbrpq_temp SET animal_name = '", animals, "';")
        invisible(dbSendQuery(conn, sql_query))
        
    }
    
    # Insert burst
     if (is.null(bursts) & length(animals) > 1) {
        
        # Use animal name as default burst name
        sql_query <- paste0("UPDATE qqbqahfsbrpq_temp
                        SET burst_name = a.",animals,"
                        FROM (
                        SELECT ",rids,", ",animals,"
                        FROM ",relocations_table,"
                        ) a
                        WHERE qqbqahfsbrpq_temp.id = a.",rids,";")
        sql_query <- gsub(pattern = '\\s', replacement = " ", x = sql_query)
        invisible(dbSendQuery(conn, sql_query))
        
    } else if (is.null(bursts) & length(animals) == 1) {
        
        sql_query <- paste0("UPDATE qqbqahfsbrpq_temp SET burst_name = '",animals,"';")
        invisible(dbSendQuery(conn, sql_query))
        
    } else if (bursts %in% fields) {
        
        # Use the field values for bursts
        sql_query <- paste0("UPDATE qqbqahfsbrpq_temp
                        SET burst_name = a.",bursts,"
                        FROM (
                        SELECT ",rids,", ",bursts,"
                        FROM ",relocations_table,"
                        ) a
                        WHERE qqbqahfsbrpq_temp.id = a.",rids,";")
        sql_query <- gsub(pattern = '\\s', replacement = " ", x = sql_query)
        invisible(dbSendQuery(conn, sql_query))
        
    } else {
        
        # Use the string
        sql_query <- paste("UPDATE qqbqahfsbrpq_temp SET burst_name = '", bursts, "';")
        invisible(dbSendQuery(conn, sql_query))
        
    }
    
    # Reset DB search path to the public schema
    sql_query <- paste0("SET search_path TO ", current_search_path, ";")
    invisible(dbSendQuery(conn, sql_query))
    
    return(TRUE)
}


