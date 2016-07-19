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
#' @param epsg Numeric. The EPSG code of the Coordinate Reference System of the 
#' relocation coordinates in the ltraj. Defaults to 0.
#' 
#' TODO check if the dataframe is an ltraj-dataframe
###############################################################################
pgTrajR2TempT <- function(conn, schema, dframe, pgtraj, epsg = NULL) {
    # Prepare the data frame to match 'relocs_temp'
    DF <- cbind(dframe, "r_id" = row.names(dframe))
    DF$p_name <- pgtraj
    i <- sapply(DF, is.factor)
    DF[i] <- lapply(DF[i], as.character)
    if ("pkey" %in% colnames(DF)) {
        DF <- DF[,c("x", "y", "id", "burst", "p_name", "r_id", "pkey")]
        names(DF) <- c("x", "y", "a_name", "b_name", "p_name", "r_id", "pkey")
    } else {
        DF <- DF[,c("x", "y", "date", "id", "burst", "p_name", "r_id")]
        names(DF) <- c("x", "y", "date", "a_name", "b_name", "p_name", "r_id")
        DF$date <- sapply(DF$date, 
                function(x) 
                    strftime(x, format = "%Y-%m-%d %H:%M:%S", tz = "", usetz = TRUE)
        )
    }
    
    # Prepare column names for insert. X and Y columns must be DF[1:2].
    x <- colnames(DF)[3:length(DF)]
    x <- c("relocation", x)
    cols <- paste0('("', paste(x, collapse = '","'), '")')
    
    # Prepare values of the dataframe for insert
    parse_row <- function(x) {
        if (all(!is.na(x[1:2]))) {
            reloc <- paste0("ST_SetSRID(ST_MakePoint(", x["x"], ",", x["y"], "),", epsg, ")")
        } else {
            reloc <- "NULL"
        }
        paste0("(",
               reloc, ", '",
               toString(paste(x[3:length(x)], collapse = "','")),
               "')")
    }
    
    d1 <- apply(DF, 1, parse_row)
    values <- paste(d1, collapse = ",")
    query_insert <- paste("INSERT INTO relocs_temp ", cols, " VALUES ", values, ";")
    # Order relocations by pkey (Type I) or by timestamp (Type II)
#    if ("pkey" %in% colnames(DF)) {
#        query_insert <- paste(query_insert, "ORDER BY pkey;")
#    } else {
#        query_insert <- paste(query_insert, "ORDER BY date;")
#    }
    
    # Begin transaction block and set database search path
    invisible(RPostgreSQL::dbGetQuery(conn, "BEGIN TRANSACTION;"))
    current_search_path <- RPostgreSQL::dbGetQuery(conn, "SHOW search_path;")
    query <- paste0("SET search_path TO ", schema, ",public;")
    invisible(RPostgreSQL::dbGetQuery(conn, query))
    
    invisible(RPostgreSQL::dbGetQuery(conn, query_insert))
    
    query <- paste0("SET search_path TO ", current_search_path, ";")
    invisible(RPostgreSQL::dbGetQuery(conn, query))
    invisible(RPostgreSQL::dbCommit(conn))
    message(paste0("Data frame successfully inserted into ", schema,".relocs_temp"))
    
    return(TRUE)
}