#' @param conn Connection object created with RPostgreSQL
#' @param relocation_data String. Name of the table that stores the relocations, e.g. "public.relocations"
#' @param relocations Vector of string(s). Name of the field(s) that contains 
#' the relocations in relocation_data. If relocations are stored as pairs of (X,Y) or 
#' (long, lat) coorindates, the coordinates should be separeted in two fields 
#' and referenced accordingly.
#' 
###############################################################################
insert_reloc_geom <- function(conn, relocation_data, relocations, timestamps=NA,
        rids) {
    # Insert relocations if trajectory Type I
    if (is.na(timestamps)) {
        # Relocations provided as point geometry
        if (length(relocations) == 1) {
            query <- paste0("INSERT INTO reloc_temp (r_id, relocation)
                            SELECT ",rids,",",relocations,"::geometry
                            FROM ",relocation_data,"
                            ORDER BY ",rids,";")
            query <- gsub(pattern = '\\s', replacement = " ", x = query)
            invisible(dbGetQuery(conn, query))
        } else if (length(relocations) == 2) {
            # Relocations provided as a coordinate pair
            x <- relocations[1]
            y <- relocations[2]
            query <- paste0("INSERT INTO reloc_temp (r_id, relocation)
                            SELECT ",rids,", ST_SetSRID(ST_MakePoint(",x,", ",y,"), ",srid,")
                            FROM ",relocation_data,"
                            ORDER BY ",rids,";")
            query <- gsub(pattern = '\\s', replacement = " ", x = query)
            invisible(dbGetQuery(conn, query))
        } else {
            stop("Relocations must be of length 1 or 2")
        }
        # If trajectory Type II
    } else {
        if (length(relocations) == 1) {
            # Relocations provided as point geometry
            query <- paste0("INSERT INTO reloc_temp (r_id, relocation, date)
                            SELECT ",rids,",",relocations,"::geometry, ",timestamps,"
                            FROM ",relocation_data,"
                            ORDER BY ",timestamps,";")
            query <- gsub(pattern = '\\s', replacement = " ", x = query)
            invisible(dbGetQuery(conn, query))
        } else if (length(relocations) == 2) {
            # Relocations provided as a coordinate pair
            x <- relocations[1]
            y <- relocations[2]
            query <- paste0("INSERT INTO reloc_temp (r_id, relocation, date)
                            SELECT ",rids,", ST_SetSRID(ST_MakePoint(",x,", ",y,"), ",srid,"), ",timestamps,"
                            FROM ",relocation_data,"
                            ORDER BY ",timestamps,";")
            query <- gsub(pattern = '\\s', replacement = " ", x = query)
            invisible(dbGetQuery(conn, query))
        } else {
            stop("Relocations must be of length 1 or 2")
        }
    }
    
}
