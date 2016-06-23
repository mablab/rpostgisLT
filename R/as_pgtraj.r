#' Function converts relocation data into the pgtraj data model.
#' 
#' Opening and closing connections have to be done manually by the user. 
#' However, the function checks if the provided connection is still valid.
#'
#' @author Balázs Dukai
#' 
#' @param conn Connection object created with RPostgreSQL
#' @param schema String. Name of the schema that stores the pgtraj data model.
#' @param relocation_data
#' @param pgtrajs
#' @param animals
#' @param bursts
#' @param relocations String. Name of the column that contains the relocations in relocation_data.
###############################################################################

as_pgtraj <- function(conn, schema = "pgtraj", relocation_data, 
        pgtrajs = "", animals = "", bursts = "", relocations) {
#    # check if conn still valid
#    if (isPostgresqlIdCurrent(conn) == TRUE) {
#        # continue
#    } else {
#        print("Database connection is not valid.")
#        return()
#    }
#    # check if relocation_data exists
#    tables <- dbListTables(conn)
#    if (relocation_data %in% tables) {
#        # continue
#    } else {
#        print(paste("Table", relocation_data, "not found in database."))
#        return()
#    }
#    # check if relocations is in relocation_data
#    columns <- dbListFields(conn, relocation_data)
#    if (relocations %in% columns) {
#        # continue
#    } else {
#        print("Column", relocations, "not found in table", relocation_data, ".")
#        return()
#    }
    # test connection, table, column and values
    testConn <- paste0("SELECT ", relocations, " FROM ", relocation_data," LIMIT 1;")
    if (is.na(dbGetQuery(conn, testConn)[1,1])) {
        print(paste("Column", relocations ,"does not contain values."))
    } else {
        # continue
    }
    
    # check if steps has SRID
    # optionally, reprojection in database could be included here but it would
    # make the code more complex, particulary with testing for valid SRID input
    testSRID <- paste0("SELECT ST_SRID(", relocations, ") FROM ", relocation_data," LIMIT 1;")
    srid <- dbGetQuery(conn, testSRID)[1,1]
    if (srid == 0) {
        acr <- NA
        while(is.na(acr) | !(acr %in% "y" | acr %in% "n")) {
            acr <- readline("The projection of the data is not defined. Do you want to continue? [y/n]")
            acr <- ifelse(grepl("y|n", acr), acr, as.character(acr))
        }
        if (acr %in% "n") {
            return()
        } else {
            # continue
        }
    } else {
        # continue
    }
    # check if schema exists
    getSchema <- "SELECT nspname FROM pg_catalog.pg_namespace;"
    schemas <- dbGetQuery(conn, getSchema)[,1]
    if (schema %in% schemas) {
        print(paste("Schema", schema ,"exists. Inserting values..."))
        # set DB search path for the schema
        s <- paste0("SET search_path TO ",schema ,",public;")
        dbSendQuery(conn, s)
        # append values to existing schema
    
    } else {
        # create schema
        pgSchema(conn, schema, display = FALSE, exec = TRUE)
        # set DB search path for the schema
        s <- paste0("SET search_path TO ",schema ,",public;")
        dbSendQuery(conn, s)
        # read and parse sql query
        createSchema <- paste(readLines("./SQL/pgtraj_schema.sql"), collapse = "\n")
        dbGetQuery(conn, createSchema)
        print(paste("Schema", schema, "created. Inserting values..."))
    }
    # reset DB search path to public
    b <- "SET search_path TO \"$user\",public;"
    dbSendQuery(conn, b)
    return(invisible())
}

