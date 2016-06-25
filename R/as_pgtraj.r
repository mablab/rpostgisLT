#' Function converts relocation data into the pgtraj data model.
#' 
#' Opening and closing connections have to be done manually by the user. 
#' However, the function checks if the provided connection is still valid.
#'
#' @authors Mathieu Basille, David Bucklin, Clément Calenge, Balázs Dukai
#' 
#' @param conn Connection object created with RPostgreSQL
#' @param schema String. Name of the schema that stores or will store the pgtraj data model.
#' @param relocation_data String. Name of the table that stores the relocations, e.g. "public.relocations"
#' @param pgtrajs String. Name of the pgtraj or name of the field that stores the pgtraj names.
#' @param animals String. Name of the animal or name of the field that stores the animal names.
#' @param bursts String. Name of the burst or name of the field that stores the burst names.
#' @param relocations String. Name of the field that contains the relocations in relocation_data.
###############################################################################

as_pgtraj <- function(conn, schema = "pgtraj", relocation_data, 
        pgtrajs, animals, bursts = NA, relocations) {
    # Initial tests ------------------------------------------------------------
    # test connection, table, field and values
    sql_conn <- paste0("SELECT ", relocations, " FROM ",
            relocation_data," LIMIT 1;")
    if (is.na(dbGetQuery(conn, sql_conn)[1,1])) {
        print(paste("Field", relocations ,"does not contain values."))
    } else {
        # continue
    }
    # check if steps has SRID
    # optionally, reprojection in database could be included here but it would
    # make the code more complex, particulary with testing for valid SRID input
    sql_srid <- paste0("SELECT ST_SRID(", relocations,
            ") FROM ", relocation_data," LIMIT 1;")
    srid <- dbGetQuery(conn, sql_srid)[1,1]
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
    sql_schemas <- "SELECT nspname FROM pg_catalog.pg_namespace;"
    schemas <- dbGetQuery(conn, sql_schemas)[,1]
    if (schema %in% schemas) {
        print(paste("Schema", schema ,
                        "exists in the database. Inserting values..."))
        # set DB search path for the pgtraj schema
        s <- paste0("SET search_path TO ",schema ,",public;")
        dbSendQuery(conn, s)
        # continue at Insert values into pgtraj
    
    } else {
        # Create pgtraj schema -------------------------------------------------
        acr <- NA
        while(is.na(acr) | !(acr %in% "y" | acr %in% "n")) {
            acr <- readline(paste("Schema", schema,
                            "does not exist in the database. Do you want to create it? [y/n]"))
            acr <- ifelse(grepl("y|n", acr), acr, as.character(acr))
        }
        if (acr %in% "n") {
            return()
        } else {
            # create schema
            pgSchema(conn, schema, display = FALSE, exec = TRUE)
            # set DB search path for the schema
            s <- paste0("SET search_path TO ",schema ,",public;")
            dbSendQuery(conn, s)
            # read and parse sql query
            sql_set_schema <- paste(readLines("./SQL/pgtraj_schema.sql"),
                    collapse = "\n")
            dbSendQuery(conn, sql_set_schema)
            print(paste("Schema", schema,
                            "created in the database. Inserting values..."))
        }
    }
    # Insert values into pgtraj ------------------------------------------------
    rd_split <- unlist(strsplit(relocation_data, "[.]"))
    # begin transaction block
    #t <- dbGetQuery(conn, "BEGIN TRANSACTION;")
    fields <- dbListFields(conn, rd_split)
    # insert pgtraj
    if (pgtrajs %in% fields) {
        # use the field values for pgtraj
        s <- paste0("INSERT INTO pgtrajs (p_name) SELECT DISTINCT ",
                pgtrajs, " FROM ", relocation_data, ";")
        dbSendQuery(conn, s)
    } else if (length(pgtrajs > 1)) {
        print(paste0("If pgtraj names are not provided in the table ",
                        relocation_data, ", pgtrajs must be of length 1."))
        return()
    } else {
        # use the string
        s <- paste0("INSERT INTO pgtrajs (p_name) VALUES ('", pgtrajs, "');")
        dbSendQuery(conn, s)
    }
    # insert animal
    if (animals %in% fields) {
        # use the field values for animal
        s <- paste0("INSERT INTO animals (a_name) SELECT DISTINCT ",
                animals, " FROM ", relocation_data, ";")
        dbSendQuery(conn, s)
    } else if (length(animals > 1)) {
        print(paste0("If animal names are not provided in the table ",
                        relocation_data, ", animals must be of length 1."))
        return()
    } else {
        # use the string
        s <- paste("INSERT INTO animals (a_name) VALUES (", animals, ");")
        dbSendQuery(conn, s)
    }
    # TODO insert burst
    if (bursts %in% fields) {
        # TODO use the field values for bursts
        s <- paste0("INSERT INTO bursts (a_name) SELECT DISTINCT ",
                animals, " FROM ", relocation_data, ";")
        dbSendQuery(conn, s)
    } else if (length(bursts > 1)) {
        print(paste0("If burst names are not provided in the table ",
                        relocation_data, ", bursts must be of length 1."))
        return()
    } else if (is.na(bursts)){
        # TODO use default burst name
        
    } else {
        # use the string
        s <- paste("INSERT INTO bursts (b_name) VALUES (", bursts, ");")
        dbSendQuery(conn, s)
    }
    # TODO insert steps into the schema

#    # commit transaction
#    if (t) {
#        dbCommit(conn)
#        print("Values were successfully inserted.")
#    } else {
#        warning("Values could not be inserted. Rolling back transaction.")
#        dbRollback(conn)
#    }
    # reset DB search path to the public schema
    s <- "SET search_path TO \"$user\",public;"
    dbGetQuery(conn, s)
    return()
}

