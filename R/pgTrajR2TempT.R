#' Insert an ltraj data frame into the 'qqbqahfsbrpq_temp' table.
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
    query_insert <- paste0("
                    INSERT INTO qqbqahfsbrpq_temp (
                        id,
                        geom,
                        relocation_time,
                        animal_name,
                        burst_name,
                        pgtraj_name)
                    SELECT
                        \"r.row.names\"::integer AS id,
                        ST_SetSRID(ST_MakePoint(x, y), ",srid,") AS geom,
                        date AS relocation_time,
                        id AS animal_name,
                        burst AS burst_name,
                        '",pgtraj,"' AS pgtraj_name
                    FROM zgaqtsn_temp;
                    ")
    query_insert <- gsub(pattern = '\\s', replacement = " ", x = query_insert)
    
    # Set database search path
    current_search_path <- dbGetQuery(conn, "SHOW search_path;")
    query <- paste0("SET search_path TO ", schema, ",public;")
    invisible(dbGetQuery(conn, query))
    
    # Insert into temporary table1
    res0 <- tryCatch({
                
        params <- c("dx", "dy", "dist", "dt", "R2n", "abs.angle", "rel.angle")
        invisible(dbWriteTable(conn, name="zgaqtsn_temp", 
                        value=dframe[, -which(names(dframe) %in% params)],
                        row.names=FALSE))
        TRUE
            
    }, warning = function(war) {
        
        message("WARNING in insert into the temporary table1:")
        message(war)
        return(war)
        
    }, error = function(err) {
        
        message("ERROR in insert into the temporary table1:")
        message(err)
        return(err)
        
    })

    
    # Insert into temporary table2
    res1 <- tryCatch({
                
        invisible(dbSendQuery(conn, query_insert))
        TRUE
        
    }, warning = function(war) {
        
        message("WARNING in insert into the temporary table2:")
        message(war)
        return(war)
        
    }, error = function(err) {
        
        message("ERROR in insert into the temporary table2:")
        message(err)
        return(err)
        
    })
    
    invisible(dbGetQuery(conn, "DROP TABLE zgaqtsn_temp;"))
    
    res <- c(res0, res1)
    
    # Restore database search path
    query <- paste0("SET search_path TO ", current_search_path, ";")
    invisible(dbSendQuery(conn, query))
    
    message(paste0("Data frame successfully inserted into ", schema,".qqbqahfsbrpq_temp"))
    
    return(all(res))
}