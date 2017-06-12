makeShinyView <- function(conn, schema, pgtraj) {
    ## Set database search path
    current_search_path <- dbGetQuery(conn, "SHOW search_path;")
    sql_query <- paste0("SET search_path TO ",
                        dbQuoteIdentifier(conn, schema), ",public;")
    invisible(dbExecute(conn, sql_query))
    
    ## Create view
    view <- dbQuoteIdentifier(conn, paste0("step_geometry_shiny_",pgtraj))
    sql_query <- paste0("
        CREATE MATERIALIZED VIEW ", view, " AS
         SELECT s.id AS step_id,
            st_transform(st_makeline(r1.geom, r2.geom), 4326) AS step_geom,
            r1.relocation_time,
            s.dt,
            s.r_rowname,
            r1.geom AS relocation1_geom,
            r2.geom AS relocation2_geom,
            ab.burst_name,
            ab.animal_name,
            p.pgtraj_name,
            ab.id AS ab_id
           FROM step s
             JOIN relocation r1 ON s.relocation_id_1 = r1.id
             JOIN relocation r2 ON s.relocation_id_2 = r2.id
             JOIN s_b_rel rel ON rel.step_id = s.id
             JOIN animal_burst ab ON ab.id = rel.animal_burst_id
             JOIN pgtraj p ON p.id = ab.pgtraj_id
          WHERE p.pgtraj_name = ",dbQuoteString(conn, pgtraj),"::text
          ORDER BY ab.id, s.id;
        
        CREATE
            INDEX step_geometry_shiny_", pgtraj, "_reloc_time_idx ON
            ", view, "
                USING btree(relocation_time);
        
        CREATE
            INDEX step_geometry_shiny_", pgtraj, "_step_geom_idx ON
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