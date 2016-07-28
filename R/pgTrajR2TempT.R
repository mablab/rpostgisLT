#' Insert an ltraj data frame into the 'relocs_temp' table.
#' Input is an ltraj converted into a data frame with ld_opt().
#' Ltraj row names are preserved. No transaction control.
#' 
#' @note ST_PointFromText() vs. ST_MakePoint() http://gis.stackexchange.com/a/122263/56083
#' 
#' @param conn Connection object created with RPostgreSQL
#' @param schema String. Name of the schema that stores or will store the pgtraj data model.
#' @param dframe Data frame created from an ltraj with the function ld_opt().
#' @param pgtraj String. Name of the new pgtraj. Defaults to the name of the 
#' variable that stores the ltraj.
#' @param srid Numeric. The PostGIS SRID of the Coordinate Reference System of the 
#' relocation coordinates in the ltraj. Defaults to 0.
#' 
#' @return TRUE on success, otherwise warning/error
#' 
###############################################################################
pgTrajR2TempT <- function(conn, schema, dframe, pgtraj, srid = 0) {
    # Prepare the data frame to match 'relocs_temp'
    colnames(dframe)[colnames(dframe) == 'r.row.names'] <- 'r_id'
    dframe$p_name <- pgtraj
    i <- sapply(dframe, is.factor)
    dframe[i] <- lapply(dframe[i], as.character)
    if ("pkey" %in% colnames(dframe)) {
        dframe <- dframe[,c("x", "y", "id", "burst", "p_name", "r_id", "pkey")]
        names(dframe) <- c("x", "y", "a_name", "b_name", "p_name", "r_id", "pkey")
    } else {
        dframe <- dframe[,c("x", "y", "date", "id", "burst", "p_name", "r_id")]
        names(dframe) <- c("x", "y", "date", "a_name", "b_name", "p_name", "r_id")
        dframe$date <- sapply(dframe$date, 
                function(x) 
                    strftime(x, format = "%Y-%m-%d %H:%M:%S", tz = "", usetz = TRUE)
        )
    }
    
    # Prepare column names for insert. X and Y columns must be dframe[1:2].
    x <- colnames(dframe)[3:length(dframe)]
    x <- c("relocation", x)
    cols <- paste0('("', paste(x, collapse = '","'), '")')
    
    # Prepare values of the dataframe for insert
    parse_row <- function(x) {
        if (all(!is.na(x[1:2]))) {
            reloc <- paste0("ST_SetSRID(ST_MakePoint(", x["x"], ",", x["y"], "),", srid, ")")
        } else {
            reloc <- "NULL"
        }
        paste0("(",
               reloc, ", '",
               toString(paste(x[3:length(x)], collapse = "','")),
               "')")
    }
    
    d1 <- apply(dframe, 1, parse_row)
    values <- paste(d1, collapse = ",")
    query_insert <- paste("INSERT INTO relocs_temp ", cols, " VALUES ", values, ";")
    # FIXME refactor 'relocs_temp' to a random name
    
    # Set database search path
    current_search_path <- dbGetQuery(conn, "SHOW search_path;")
    query <- paste0("SET search_path TO ", schema, ",public;")
    invisible(dbGetQuery(conn, query))
    
    res <- tryCatch({
                
        invisible(dbSendQuery(conn, query_insert))
        return(TRUE)
        
    }, warning = function(war) {
        
        message("WARNING in insert into the temporary table:")
        message(war)
        return(war)
        
    }, error = function(err) {
        
        message("ERROR in insert into the temporary table:")
        message(err)
        return(err)
        
    })
    
    # Restore database search path
    query <- paste0("SET search_path TO ", current_search_path, ";")
    invisible(dbSendQuery(conn, query))
    
    message(paste0("Data frame successfully inserted into ", schema,".relocs_temp"))
    
    return(res)
}