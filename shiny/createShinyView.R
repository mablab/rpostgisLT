#' Create a materialized view of the steps of a pgtraj for the shiny app
#' 
#' Steps are projected to EPSG:4326 thus there is no need for coordinate transformation
#' for leaflet.
#'
#' @param conn DBI::DBIConnection
#' @param schema String. Schema name.
#' @param pgtraj String. Pgtraj name.
#'
#' @return nothing
#'
#' @author Balázs Dukai \email{balazs.dukai@@gmail.com}
#' @keywords internal
createShinyStepsView <- function(conn, schema, pgtraj) {
    ## Set database search path
    current_search_path <- dbGetQuery(conn, "SHOW search_path;")
    sql_query <- paste0("SET search_path TO ",
                        dbQuoteIdentifier(conn, schema),
                        ",public;")
    invisible(dbExecute(conn, sql_query))
    
    pgtraj_s <- dbQuoteString(conn, pgtraj)
    view <-
        dbQuoteIdentifier(conn, paste0("step_geometry_shiny_", pgtraj))
    
    infolocs_table <- paste0("infolocs_", pgtraj)
    info_cols <- getInfolocsColumns(conn, schema, infolocs_table)
    
    # if there is an infolocs table
    if (nrow(info_cols) > 0) {
        cols <- paste(paste(paste0(
            "i.",
            dbQuoteIdentifier(conn, info_cols$column_name)
        ),
        collapse = ", "),
        ",")
        join <-
            paste0("JOIN ", infolocs_table, " i ON p.step_id = i.step_id")
    } else {
        cols <- NULL
        join <- NULL
    }
    
    # Stop in case the relocations are not projected, because Leaflet cannot plot them
    sql_query <-
        paste0("SELECT proj4string FROM pgtraj WHERE pgtraj_name = ",
               pgtraj_s,
               ";")
    srid <- dbGetQuery(conn, sql_query)$proj4string
    if (is.na(srid)) {
        stop("Cannot plot unprojected geometries (0 SRID). Not creating MATERIALIZED VIEW.")
    }
    
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
                ",cols,"
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
    
    
    message(paste0("MATERIALIZED VIEW 'step_geometry_shiny_",pgtraj,"' created in schema '",
                   schema, "'."))
    
    dbVacuum(conn, name = paste0("step_geometry_shiny_",pgtraj), analyze = TRUE)
    
    ## Restore database search path
    sql_query <- paste0("SET search_path TO ", current_search_path, ";")
    invisible(dbExecute(conn, sql_query))
}


#' Create a materialized view of all bursts for the shiny app
#'
#' It is expected that *all* pgtrajes are projected in the schema in order to
#' run. Bursts are projected to EPSG:4326 thus there is no need for coordinate transformation
#' for leaflet.
#'
#' @param conn DBI::DBIConnection
#' @param schema String. Schema name.
#'
#' @return nothing
#'
#' @author Balázs Dukai \email{balazs.dukai@@gmail.com}
#' @keywords internal
createShinyBurstsView <- function(conn, schema) {
    ## Set database search path
    current_search_path <- dbGetQuery(conn, "SHOW search_path;")
    sql_query <- paste0("SET search_path TO ",
                        dbQuoteIdentifier(conn, schema),
                        ",public;")
    invisible(dbExecute(conn, sql_query))
    
    view <- dbQuoteIdentifier(conn, "all_burst_summary_shiny")
    
    # Stop in case the relocations are not projected, because Leaflet cannot plot them
    sql_query <- paste0("SELECT proj4string FROM pgtraj LIMIT 1;")
    srid <- dbGetQuery(conn, sql_query)$proj4string
    if (is.na(srid)) {
        stop("Cannot plot unprojected geometries (0 SRID). Not creating MATERIALIZED VIEW.")
    }
    
    sql_query <- paste0("
                        CREATE
                            MATERIALIZED VIEW IF NOT EXISTS ", view, " AS SELECT
                                p.id AS pgtraj_id,
                                p.pgtraj_name,
                                ab.animal_name,
                                ab.burst_name,
                                COUNT( r.id ) AS num_relocations,
                                COUNT( r.id )- COUNT( r.geom ) AS num_na,
                                MIN( r.relocation_time ) AS date_begin,
                                MAX( r.relocation_time ) AS date_end,
                                st_transform(
                                    st_makeline(r.geom),
                                    4326
                                )::geometry(
                                    LineString,
                                    4326
                                ) AS burst_geom
                            FROM
                                pgtraj p,
                                animal_burst ab,
                                relocation r,
                                s_b_rel sb,
                                step s
                            WHERE
                                p.id = ab.pgtraj_id
                                AND ab.id = sb.animal_burst_id
                                AND sb.step_id = s.id
                                AND s.relocation_id_1 = r.id
                            GROUP BY
                                p.id,
                                p.pgtraj_name,
                                ab.id,
                                ab.animal_name,
                                ab.burst_name
                            ORDER BY
                                p.id,
                                ab.id;
                        
            CREATE
            INDEX IF NOT EXISTS all_burst_summary_shiny_burst_name_idx ON
            all_burst_summary_shiny USING btree(burst_name);")
    
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
    
    
    message(paste0("MATERIALIZED VIEW all_burst_summary_shiny created in schema '",
                   schema, "'."))
    
    dbVacuum(conn, name = "all_burst_summary_shiny", analyze = TRUE)
    
    ## Restore database search path
    sql_query <- paste0("SET search_path TO ", current_search_path, ";")
    invisible(dbExecute(conn, sql_query))
}


