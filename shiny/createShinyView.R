
# get all columns in the infolocs table but the step_id
getInfolocsColumns <- function(conn, schema, infolocs_table){
    schema_s <- dbQuoteString(conn, schema)
    table_s <- dbQuoteString(conn, infolocs_table)
    
    sql_query <- paste0("
                        SELECT column_name
                        FROM information_schema.columns
                        WHERE table_schema = ",schema_s,"
                        AND table_name = ",table_s,"
                        AND column_name != 'step_id';")
    
    return(dbGetQuery(conn, sql_query))
}


createShinyView <- function(conn, schema, pgtraj) {
    ## Set database search path
    current_search_path <- dbGetQuery(conn, "SHOW search_path;")
    sql_query <- paste0("SET search_path TO ",
                        dbQuoteIdentifier(conn, schema), ",public;")
    invisible(dbExecute(conn, sql_query))
    
    pgtraj_s <- dbQuoteString(conn, pgtraj)
    view <- dbQuoteIdentifier(conn, paste0("step_geometry_shiny_",pgtraj))
    
    infolocs_table <- paste0("infolocs_", pgtraj)
    info_cols <- getInfolocsColumns(conn, schema, infolocs_table)
    
    # if there is an infolocs table
    if (nrow(info_cols) > 0) {
        cols <- paste(paste0("i.",
                             dbQuoteIdentifier(conn, info_cols$column_name)),
                      collapse = ", ")
        join <-
            paste0("JOIN ", infolocs_table, " i ON p.step_id = i.step_id")
    } else {
        cols <- NULL
        join <- NULL
    }
    
    # In case the relocations are not projected
    sql_query <- paste0("SELECT proj4string FROM pgtraj WHERE pgtraj_name = ", pgtraj_s,";")
    srid <- dbGetQuery(conn, sql_query)$proj4string
    
    if(is.na(srid)) {
        step_geom <- "st_makeline(r1.geom, r2.geom) AS step_geom"
    } else {
        step_geom <- "st_transform(st_makeline(r1.geom, r2.geom), 4326)::geometry(LineString,4326) AS step_geom"
    }
    
    sql_query <- paste0("
        CREATE MATERIALIZED VIEW IF NOT EXISTS ", view, " AS
            SELECT
                p.step_id,
                ",step_geom,",
                r1.relocation_time AS date,
                p.dx,
                p.dy,
                p.dist,
                p.dt,
                p.abs_angle,
                p.rel_angle,
                ",cols,",
                p.animal_name,
                p.burst AS burst_name,
                p.pgtraj AS pgtraj_name
            FROM parameters_",pgtraj," p
                JOIN step s ON p.step_id = s.id
                 JOIN relocation r1 ON s.relocation_id_1 = r1.id
                 JOIN relocation r2 ON s.relocation_id_2 = r2.id
                ",join,"
            WHERE st_makeline(r1.geom, r2.geom) NOTNULL;
        
        CREATE
            INDEX IF NOT EXISTS step_geometry_shiny_", pgtraj, "_date_idx ON
            ", view, "
                USING btree(date);
        
        CREATE
            INDEX IF NOT EXISTS step_geometry_shiny_", pgtraj, "_step_geom_idx ON
            ", view, "
                USING gist(step_geom);")
    
    create_sql_query <- gsub(pattern = '\\s', replacement = " ",
                             x = sql_query)
    
    tryCatch({
        invisible(dbExecute(conn, create_sql_query))
        TRUE
    }, warning = function(x) {
        message(x)
        message(" . Cannot CREATE MATERIALIZED VIEW")
        ## Restore database search path
        sql_query <- paste0("SET search_path TO ", current_search_path, ";")
        invisible(dbExecute(conn, sql_query))
        stop("Returning from function")
        
    }, error = function(x) {
        message(x)
        message(". Cannot CREATE MATERIALIZED VIEW")
        ## Restore database search path
        sql_query <- paste0("SET search_path TO ", current_search_path, ";")
        invisible(dbExecute(conn, sql_query))
        stop("Returning from function")
    })
    
    # tryCatch(if(invisible(dbExecute(conn, create_sql_query))),
    #          error = function() {
    #              sql_query <- paste0("SET search_path TO ", current_search_path, ";")
    #              invisible(dbExecute(conn, sql_query))
    #              
    #              stop()
    #                 
    #          })
    
    message(paste0("MATERIALIZED VIEW 'step_geometry_shiny_",pgtraj,"' created in schema '",
                   schema, "'."))
    
    dbVacuum(conn, name = paste0("step_geometry_shiny_",pgtraj), analyze = TRUE)
    
    ## Restore database search path
    sql_query <- paste0("SET search_path TO ", current_search_path, ";")
    invisible(dbExecute(conn, sql_query))
}