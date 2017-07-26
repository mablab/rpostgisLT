
# check if there are infolocs for a pgtraj
checkInfolocs <- function(conn, schema, infolocs_table) {
    schema_s <- dbQuoteString(conn, schema)
    table_s <- dbQuoteString(conn, infolocs_table)
    
    sql_query <- paste0("
                        SELECT table_name
                        FROM information_schema.tables
                        WHERE table_schema = ",schema_s,"
                        AND table_name = ",table_s,";")
    x <- dbGetQuery(conn, sql_query)
    
    if(nrow(x) == 1){
        return(TRUE)
    } else {
        return(FALSE)
    }
}

# get all columns in the infolocs table but the step_id
getInfolcsColumns <- function(conn, schema, infolocs_table){
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


makeShinyView <- function(conn, schema, pgtraj) {
    ## Set database search path
    current_search_path <- dbGetQuery(conn, "SHOW search_path;")
    sql_query <- paste0("SET search_path TO ",
                        dbQuoteIdentifier(conn, schema), ",public;")
    invisible(dbExecute(conn, sql_query))
    
    infolocs_table <- paste0("infolocs_", pgtraj)
    info_true <- checkInfolocs(conn, schema, infolocs_table)
    
    ## Create view
    view <- dbQuoteIdentifier(conn, paste0("step_geometry_shiny_",pgtraj))
    sql_query <- paste0("
        CREATE MATERIALIZED VIEW IF NOT EXISTS ", view, " AS
            SELECT
                p.step_id,
                st_transform(st_makeline(r1.geom, r2.geom), 4326)::geometry(LineString,4326) AS step_geom,
                r1.relocation_time AS date,
                p.dx,
                p.dy,
                p.dist,
                p.dt,
                p.abs_angle,
                p.rel_angle,
                p.animal_name,
                p.burst AS burst_name,
                p.pgtraj AS pgtraj_name
            FROM parameters_",pgtraj," p
                JOIN step s ON p.step_id = s.id
                 JOIN relocation r1 ON s.relocation_id_1 = r1.id
                 JOIN relocation r2 ON s.relocation_id_2 = r2.id
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
    invisible(dbExecute(conn, create_sql_query))
    message(paste0("View 'step_geometry_shiny_",pgtraj,"' created in schema '",
                   schema, "'."))
    
    dbVacuum(conn, name = paste0("step_geometry_shiny_",pgtraj), analyze = TRUE)
    
    ## Restore database search path
    sql_query <- paste0("SET search_path TO ", current_search_path, ";")
    invisible(dbExecute(conn, sql_query))
}