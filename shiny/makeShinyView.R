makeShinyView <- function(conn, schema, pgtraj) {
    ## Set database search path
    current_search_path <- dbGetQuery(conn, "SHOW search_path;")
    sql_query <- paste0("SET search_path TO ",
                        dbQuoteIdentifier(conn, schema), ",public;")
    invisible(dbExecute(conn, sql_query))
    
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