#' Creates the table 'reloc_temp', which is a temporary table used by other
#' function of the package.
#' 
#' @param conn Connection object created with RPostgreSQL
#' @param schema String. Name of the schema that stores or will store the pgtraj data model.
#' 
###############################################################################
make_reloc_temp <- function(conn, schema) {
    # Create 'reloc_temp' table
    query <- paste0("CREATE TABLE ", schema, ".reloc_temp (
                    r_id    integer,
                    relocation     geometry,
                    date    timestamptz,
                    b_name      text,
                    a_name      text,
                    p_name      text
                    );")
    query <- gsub(pattern = '\\s', replacement = " ", x = query)
    invisible(dbGetQuery(conn, query))
}