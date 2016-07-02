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
#' @param timestamp String. Name of the field in relocation_data that contains the timestamps.
#' @param rid String. Name of the field in relocation_data that contains the numeric IDs of relocations.
#' 
#' Not tested with capital letters for PostgreSQL field names, but it probably won't 
#' work. Its a bad practice anyway to force uppercase in PostgreSQL so use lowercase.

# TODO test capital letters in field names
# TODO separate create schema, include pgtraj_schema.sql
# TODO supressWarning might work for geometry warnings
# TODO check external code folder for R package
# TODO subset raw data 
# TODO include smaller logical blocks
# line end comment
## below line comment
### standalone


###############################################################################

as_pgtraj <- function(conn, schema = "pgtraj", relocation_data, 
        pgtrajs = "pgtraj", animals = "animal", bursts = NA, relocations,
        timestamp = NA, rid = "rid") {
    # Initial tests ------------------------------------------------------------
    # Test connection, table, field and values
    sql_conn <- paste0("SELECT ", relocations, " FROM ",
            relocation_data," LIMIT 1;")
    if (is.na(dbGetQuery(conn, sql_conn)[1,1])) {
        print(paste("Field", relocations ,"does not contain values."))
    }
    
    # Check if steps has SRID
    # Optionally, reprojection in database could be included here but it would
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
        }
    }
    
    # Check if schema exists
    # TODO Check if also all necessary tables exist
    sql_schemas <- "SELECT nspname FROM pg_catalog.pg_namespace;"
    schemas <- dbGetQuery(conn, sql_schemas)[,1]
    if (schema %in% schemas) {
        print(paste("Schema", schema ,
                        "exists in the database. Inserting values..."))
        # Set DB search path for the pgtraj schema
        s <- paste0("SET search_path TO ",schema ,",public;")
        dbSendQuery(conn, s)
        # Continue at Insert values into pgtraj
    
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
            # Create schema
            pgSchema(conn, schema, display = FALSE, exec = TRUE)
            # Set DB search path for the schema
            s <- paste0("SET search_path TO ",schema ,",public;")
            dbSendQuery(conn, s)
            # Read and parse sql query
            sql_set_schema <- paste(readLines("./Scripts/pgtraj_schema.sql"),
                    collapse = "\n")
            dbSendQuery(conn, sql_set_schema)
            print(paste("Schema", schema,
                            "created in the database. Inserting values..."))
        }
    }
    # Insert values into pgtraj ------------------------------------------------
    rd_split <- unlist(strsplit(relocation_data, "[.]"))
    # Begin transaction block
    t <- dbGetQuery(conn, "BEGIN TRANSACTION;")
    fields <- dbListFields(conn, rd_split)
    # Create the temporary table from raw data
    sql_temp <- paste0("CREATE TABLE relocs_temp (
                            r_id    integer,
                            relocation     geometry,
                            date    timestamptz,
                            b_name      text,
                            a_name      text,
                            p_name      text
                        );")
    sql_temp <- gsub(pattern = '\\s', replacement = " ", x = sql_temp)
    t <- c(t, dbGetQuery(conn, sql_temp))
    
    # Filling the temporary table-----------------------------------------------
    # Insert relocations if trajectory Type I
    if (is.na(timestamp)) {
        sql_reloc <- paste0("INSERT INTO relocs_temp (r_id, relocation)
                        SELECT ",rid,",",relocations,"::geometry
                        FROM ",relocation_data,"
                        ORDER BY ",rid,";")
        sql_reloc <- gsub(pattern = '\\s', replacement = " ", x = sql_reloc)
        t <- c(t, dbGetQuery(conn, sql_reloc))
    # If trajectory Type II
    } else {
        sql_reloc <- paste0("INSERT INTO relocs_temp (r_id, relocation, date)
                        SELECT ",rid,",",relocations,"::geometry, ",timestamp,"
                        FROM ",relocation_data,"
                        ORDER BY ",timestamp,";")
        sql_reloc <- gsub(pattern = '\\s', replacement = " ", x = sql_reloc)
        t <- c(t, dbGetQuery(conn, sql_reloc))
    }
    
    # Insert pgtraj
    if (pgtrajs %in% fields) {
        # use the field values for pgtraj
        s <- paste0("UPDATE relocs_temp
                    SET p_name = a.",pgtrajs,"
                    FROM (
                        SELECT ",rid,", ",pgtrajs,"
                        FROM ",relocation_data,"
                    ) a
                    WHERE r_id = a.",rid,";")
        s <- gsub(pattern = '\\s', replacement = " ", x = s)
        dbSendQuery(conn, s)
    } else if (length(pgtrajs) > 1) {
        print(paste0("If pgtraj names are not provided in the table ",
                        relocation_data, ", pgtrajs must be of length 1."))
        dbRollback(conn)
        return()
    } else {
        # Use the string
        s <- paste0("UPDATE relocs_temp SET p_name = '", pgtrajs, "';")
        dbSendQuery(conn, s)
    }
    
    # Insert animal
    if (animals %in% fields) {
        # Use the field values for animal
        s <- paste0("UPDATE relocs_temp
                    SET a_name = a.",animals,"
                    FROM (
                        SELECT ",rid,", ",animals,"
                        FROM ",relocation_data,"
                        ) a
                    WHERE r_id = a.",rid,";")
        s <- gsub(pattern = '\\s', replacement = " ", x = s)
        dbSendQuery(conn, s)
    } else if (length(animals) > 1) {
        print(paste0("If animal names are not provided in the table ",
                        relocation_data, ", animals must be of length 1."))
        dbRollback(conn)
        return()
    } else {
        # Use the string
        s <- paste("UPDATE relocs_temp SET a_name = '", animals, "';")
        dbSendQuery(conn, s)
    }
    
    # Insert burst
    if (bursts %in% fields) {
        # Use the field values for bursts
        s <- paste0("UPDATE relocs_temp
                    SET b_name = a.",bursts,"
                    FROM (
                        SELECT ",rid,", ",bursts,"
                        FROM ",relocation_data,"
                        ) a
                    WHERE r_id = a.",rid,";")
        s <- gsub(pattern = '\\s', replacement = " ", x = s)
        dbSendQuery(conn, s)
    } else if (length(bursts) > 1) {
        print(paste0("If burst names are not provided in the table ",
                        relocation_data, ", bursts must be of length 1."))
        dbRollback(conn)
        return()
    } else if (is.na(bursts) & length(animals) > 1){
        # Use animal name as default burst name
        s <- paste0("UPDATE relocs_temp
                        SET b_name = a.",animals,"
                        FROM (
                        SELECT ",rid,", ",animals,"
                        FROM ",relocation_data,"
                        ) a
                        WHERE r_id = a.",rid,";")
        s <- gsub(pattern = '\\s', replacement = " ", x = s)
        dbSendQuery(conn, s)
    } else if (is.na(bursts) & length(animals) == 1) {
        s <- paste0("UPDATE relocs_temp SET b_name = '",animals,"';")
        dbSendQuery(conn, s)
    } else {
        # Use the string
        s <- paste("INSERT INTO bursts (b_name) VALUES ('", bursts, "');")
        dbSendQuery(conn, s)
    }
    
    # From here insert the into pgtraj from the temporary table
    sql_insert <- paste0("INSERT INTO pgtrajs (p_name)
                        SELECT DISTINCT p_name
                        FROM relocs_temp;
                        
                        INSERT INTO animals (a_name)
                        SELECT DISTINCT a_name
                        FROM relocs_temp;
                        
                        INSERT INTO bursts (b_name, a_id)
                        SELECT DISTINCT a.b_name, b.a_id
                        FROM relocs_temp a JOIN animals b
                        ON a.a_name = b.a_name;
                        
                        INSERT INTO p_b_rel (p_id, b_id)
                        SELECT DISTINCT b.p_id, b_id
                        FROM relocs_temp a 
                            JOIN pgtrajs b
                                ON a.p_name = b.p_name
                            JOIN bursts c
                                ON a.b_name = c.b_name;")
    sql_insert <- gsub(pattern = '\\s', replacement = " ", x = sql_insert)
    t <- c(t, dbGetQuery(conn, sql_insert))
    
    # Insert steps into the schema
    bst <- dbGetQuery(conn,"SELECT DISTINCT b_name FROM relocs_temp;")[,1]
    t <- c(t, ifelse(is.null(bst), FALSE, TRUE))
    # TODO add t test
    dbGetQuery(conn,"ALTER TABLE steps ADD b_name text;")
    for (i in bst) {
        sql_steps <- paste0("INSERT INTO steps (
                                step,
                                date,
                                dt,
                                b_name
                            ) (
                                SELECT 
                                    st_makeline(a.relocation, b.relocation) AS step,
                                    a.date,
                                    b.date - a.date AS dt,
                                    a.b_name
                                FROM 
                                    relocs_temp AS a
                                    INNER JOIN relocs_temp AS b 
                                        ON a.r_id + 1 = b.r_id AND
                                        a.b_name = b.b_name
                                WHERE a.b_name = '", i,
                                "' ORDER BY a.r_id
                            );")
        sql_steps <- gsub(pattern = '\\s', replacement = " ", x = sql_steps)
        t <- c(t, dbGetQuery(conn, sql_steps))
        # Insert step-burst relations
        sql_rel <- paste0("INSERT INTO s_i_b_rel (s_id, b_id)
                        SELECT a.s_id, b.b_id
                        FROM steps a, bursts b
                        WHERE a.b_name = '",i , "' 
                        AND b.b_name = '", i, "'; "
                        )
        sql_rel <- gsub(pattern = '\\s', replacement = " ", x = sql_rel)
        t <- c(t, dbGetQuery(conn, sql_rel))
        # Delete b_names from temporary column
        t <- c(t, dbGetQuery(conn, "UPDATE steps SET b_name = NULL;"))
    }
    
    # TODO add t test
    # Drop temporary column and table
    dbGetQuery(conn,"ALTER TABLE steps DROP COLUMN b_name;")
    dbGetQuery(conn, "DROP TABLE relocs_temp;")
    # Commit transaction
    if (all(t)) {
        dbCommit(conn)
        print("Values were successfully inserted.")
    } else {
        print("Values could not be inserted. Rolling back transaction.")
        dbRollback(conn)
    }
    # Reset DB search path to the public schema
    s <- "SET search_path TO \"$user\",public;"
    dbGetQuery(conn, s)
    return()
}

