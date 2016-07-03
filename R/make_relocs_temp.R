#' Creates the table 'relocs_temp', which is a temporary table used by other
#' function of the package.
#' 
#' @param conn Connection object created with RPostgreSQL
#' @param schema String. Name of the schema that stores or will store the pgtraj data model.
#' 
###############################################################################
make_relocs_temp <- function(conn, schema) {
    # Create 'relocs_temp' table
    query <- paste0("CREATE TABLE ", schema, ".relocs_temp (
                    r_id    integer,
                    relocation     geometry,
                    date    timestamptz,
                    b_name      text,
                    a_name      text,
                    p_name      text
                    );")
    query <- gsub(pattern = '\\s', replacement = " ", x = query)
    # Create sequence for r_id
    query2 <- paste0("CREATE SEQUENCE ", schema, ".temp_r_id_seq;")
    query <- paste(query, query2)
    
    invisible(dbGetQuery(conn, query))
}

drop_relocs_temp <- function(conn, schema) {
    query <- paste0("DROP TABLE ", schema, ".relocs_temp;")
    query2 <- paste0("DROP SEQUENCE ", schema, ".temp_r_id_seq;")
    query <- paste(query, query2)
    invisible(dbGetQuery(conn, query))
}