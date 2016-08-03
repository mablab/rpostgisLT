#' Creates a temporary table in the 'traj' schema.
#' 
#' @description
#' Used by \code{pgTrajDB2TempT} and \code{pgTrajR2TempT} to create a temporary
#' table which will be populated by these functions. The temporary table's
#' name is a random string to avoid collation with user generated tables.
#' 
#' @author Balázs Dukai \email{balazs.dukai@@gmail.com}
#' 
#' @param conn Connection object created with RPostgreSQL
#' @param schema String. Name of the schema that stores or will store the pgtraj data model.
#' 
#' @return TRUE on success, otherwise warning/error
#' 
#' @examples
#' \dontrun{pgTrajTempT(conn, "traj_1")}
#' 
###############################################################################
pgTrajTempT <- function(conn, schema) {
    # Check if table already exists
    query <- paste0("SELECT * FROM pg_tables WHERE schemaname = '", schema, "';")
    tables <- invisible(dbGetQuery(conn, query))
    if ('qqbqahfsbrpq_temp' %in% tables$tablename) {
        acr <- NA
        while(is.na(acr) | !(acr %in% "y" | acr %in% "n")) {
            acr <- readline(paste("A table named 'qqbqahfsbrpq_temp' already exists in the schema", schema,
                            ". Do you want to delete it? [y/n]"))
            acr <- ifelse(grepl("y|n", acr), acr, as.character(acr))
        }
        if (acr %in% "n") {
            return(FALSE)
        } else {
            query <- paste0("DROP TABLE IF EXISTS ", schema, ".qqbqahfsbrpq_temp;")
            invisible(dbSendQuery(conn, query))
        }
    }
    
    # Create 'qqbqahfsbrpq_temp' table
    query <- paste0("CREATE TABLE ", schema, ".qqbqahfsbrpq_temp (
                    id               serial,
                    pkey             text,
                    geom             geometry,
                    relocation_time  timestamptz,
                    burst_name       text,
                    animal_name      text,
                    pgtraj_name      text
                    );")
    create_query <- gsub(pattern = '\\s', replacement = " ", x = query)
    
    res <- tryCatch({
        
        invisible(dbSendQuery(conn, create_query))
        return(TRUE)
        
    }, warning = function(war) {
        
        message("WARNING in creating the temporary table:")
        message(war)
        return(war)
        
    }, error = function(err) {
        
        message("ERROR in creating the temporary table:")
        message(err)
        return(err)
        
    })
    
    return(res)
    
}