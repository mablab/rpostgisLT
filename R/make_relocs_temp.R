#' Creates the table 'relocs_temp', which is a temporary table used by other
#' function of the package.
#' 
#' @author Balázs Dukai
#' 
#' @param conn Connection object created with RPostgreSQL
#' @param schema String. Name of the schema that stores or will store the pgtraj data model.
#' 
###############################################################################
drop_relocs_temp <- function(conn, schema) {
    query <- paste0("DROP TABLE IF EXISTS ", schema, ".relocs_temp;")
    invisible(dbGetQuery(conn, query))
}

make_relocs_temp <- function(conn, schema) {
    # Check if table already exists
    query <- paste0("SELECT * FROM pg_tables WHERE schemaname = '", schema, "';")
    tables <- invisible(dbGetQuery(conn, query))
    if ('relocs_temp' %in% tables$tablename) {
        acr <- NA
        while(is.na(acr) | !(acr %in% "y" | acr %in% "n")) {
            acr <- readline(paste("The table 'relocs_temp' already exists in the schema", schema,
                            ". Do you want to delete the table? [y/n]"))
            acr <- ifelse(grepl("y|n", acr), acr, as.character(acr))
        }
        if (acr %in% "n") {
            return(FALSE)
        } else {
            drop_relocs_temp(conn, schema)
        }
    }
    
    # Create 'relocs_temp' table
    query <- paste0("CREATE TABLE ", schema, ".relocs_temp (
                    r_id    serial,
                    pkey    text,
                    relocation     geometry,
                    date    timestamptz,
                    b_name      text,
                    a_name      text,
                    p_name      text
                    );")
    query <- gsub(pattern = '\\s', replacement = " ", x = query)
    invisible(dbGetQuery(conn, query))
}