#' Computes the trajectory parameters (as in ltraj) for a pgtraj and creates a 
#' view for the pgtraj. The views are always named as '<pgtraj_name>_parameters'.
#' 
#' @author Balázs Dukai
#' 
#' @param conn Connection object created with RPostgreSQL
#' @param schema String. Name of the schema that stores or will store the pgtraj data model.
#' @param pgtraj String. Name of the pgtraj.
#' @param epsg Numeric. EPSG code of the relocation geometry.
#' @param db Boolean. A switch that controlls the parameters view creation 
#' depending on source of data (R or PostgreSQL). If TRUE, raw data input from
#' a database table is assumed. In this case all parameters will be computed.
#' If FALSE, it is assumed that an ltraj was input from R with already computed
#' parameters. In this case R2n and rel.angle will not be recomputed, but
#' reused from the ltraj.
#' 
#' @return TRUE on success, otherwise warning/error
#' 
##############################################################################
pgTrajViewParams <- function(conn, schema, pgtraj, epsg, db = TRUE) {
    
    current_search_path <- dbGetQuery(conn, "SHOW search_path;")
    sql_query <- paste0("SET search_path TO ", schema, ",public;")
    invisible(dbGetQuery(conn, sql_query))
    
    if (db) {
        sql_query <- paste0(
        "CREATE OR REPLACE VIEW ",pgtraj,"_parameters AS
        WITH step_geom AS (
            SELECT
                s.id AS step_id,
                ST_Makeline(r1.geom, r2.geom) AS step_geom,
                r1.relocation_time,
                s.dt,
                r1.r_rowname,
                r1.geom AS relocation1_geom,
                r2.geom AS relocation2_geom,
                ab.burst_name,
                ab.animal_name,
                p.pgtraj_name,
                ab.id AS ab_id
            FROM step s
            JOIN relocation r1 ON s.relocation_id_1 = r1.id
            LEFT JOIN relocation r2 ON s.relocation_id_2 = r2.id
            JOIN s_i_b_rel rel ON rel.step_id = s.id
            JOIN animal_burst ab ON ab.id = rel.animal_burst_id
            JOIN pgtraj p ON p.id = ab.pgtraj_id
            WHERE p.pgtraj_name = '",pgtraj,"'
            ),
        step_shape AS (
            SELECT
                t.step_id,
                t.step_geom,
                t.relocation_time,
                t.dt,
                t.r_rowname,
                t.dx,
                t.dy,
                t.relocation1_geom,
                t.relocation2_geom,
                t.burst_name,
                t.animal_name,
                t.pgtraj_name,
                t.ab_id,
                t.dist,
                CASE
                    WHEN t.dist < 1e-07 THEN NULL
                    WHEN t.dist >= 1e-07 THEN atan2(t.dy, t.dx)
                END AS abs_angle
            FROM (SELECT
                      step_id,
                      step_geom,
                      relocation_time,
                      dt,
                      r_rowname,
                      relocation1_geom,
                      relocation2_geom,
                      burst_name,
                      animal_name,
                      pgtraj_name,
                      ab_id,
                      ST_length(step_geom) AS dist,
                      ST_X(relocation2_geom) - ST_X(relocation1_geom) AS dx,
                      ST_Y(relocation2_geom) - ST_Y(relocation1_geom) AS dy
                  FROM step_geom
                 ) AS t
            ),
        step_parameters AS (
            SELECT
                a.step_id,
                a.r_rowname,
                ST_x(a.relocation1_geom) AS x, 
                ST_y(a.relocation1_geom) AS y,
                a.relocation_time AS date,
                a.dx,
                a.dy,
                a.dist,
                extract(epoch FROM a.dt) AS dt,
                ST_Distance(startp.relocation1_geom, a.relocation1_geom)^2 AS r2n,
                a.abs_angle,
                a.animal_name,
                a.burst_name AS burst,
                a.pgtraj_name AS pgtraj
            FROM step_shape AS a
            JOIN (SELECT
                    ab_id,
                    relocation1_geom
                  FROM step_shape
                  WHERE step_id IN (SELECT MIN(step_id) 
                                    FROM step_shape 
                                    GROUP BY ab_id)
                  ) AS startp 
                  ON startp.ab_id = a.ab_id
            ),
        r_angle AS (
            SELECT
                s2.step_id,
                CASE
                    WHEN s.dist < 1e-07 THEN s2.abs_angle - (SELECT sub.abs_angle
                                                            FROM step_shape AS sub
                                                            WHERE sub.step_id <= s.step_id
                                                              AND sub.dist >= 1e-07
                                                            ORDER BY sub.step_id DESC
                                                            LIMIT 1)
                    ELSE s2.abs_angle - s.abs_angle
                END AS rel_angle
            FROM step_shape AS s
            LEFT OUTER JOIN LATERAL (SELECT *
                               FROM step_shape c
                               WHERE s.step_id < c.step_id
                               AND s.burst_name = c.burst_name
                               LIMIT 1
                              ) AS s2
            ON TRUE
        )
        SELECT
            p.r_rowname,
            p.x, 
            p.y,
            p.date,
            p.dx,
            p.dy,
            p.dist,
            p.dt,
            p.r2n,
            p.abs_angle,
            CASE
                WHEN r_angle.rel_angle <= -pi() THEN 2 * pi() + r_angle.rel_angle
                WHEN r_angle.rel_angle > pi() THEN r_angle.rel_angle - 2 * pi()
                ELSE r_angle.rel_angle
            END AS rel_angle,
            p.animal_name,
            p.burst,
            p.pgtraj
        FROM step_parameters AS p
        LEFT OUTER JOIN r_angle
        ON r_angle.step_id = p.step_id
        ORDER BY p.burst, p.date;"
        )
        create_sql_query <- gsub(pattern = '\\s', replacement = " ",
                x = sql_query)
        
        res <- tryCatch({
                    
                    invisible(dbSendQuery(conn, create_sql_query))
                    message(paste0("View '", pgtraj,
                                    "_parameters' created in schema '",
                                    schema, "'."))
                    return(TRUE)
                    
                }, warning = function(war) {
                    
                    message(paste0("WARNING in creating view '",
                                    pgtraj,"_parameters' :"))
                    message(war)
                    return(war)
                    
                }, error = function(err) {
                    
                    message(paste0("ERROR in creating view '",
                                    pgtraj,"_parameters' :"))
                    message(err)
                    return(err)
                    
                })
    } else {
        sql_query <- paste0(
        "CREATE OR REPLACE VIEW ",pgtraj,"_parameters AS
        -- create LINESTRING geometries from successive relocations
        WITH step_geom AS (
            SELECT
                s.id AS step_id,
                ST_Makeline(r1.geom, r2.geom) AS step_geom,
                r1.relocation_time,
                s.dt,
                s.r_rowname,
                r1.geom AS relocation1_geom,
                r2.geom AS relocation2_geom,
                s.r2n,
                s.rel_angle,
                ab.burst_name,
                ab.animal_name,
                p.pgtraj_name,
                ab.id AS ab_id
            FROM step s
            JOIN relocation r1 ON s.relocation_id_1 = r1.id
            LEFT JOIN relocation r2 ON s.relocation_id_2 = r2.id
            JOIN s_i_b_rel rel ON rel.step_id = s.id
            JOIN animal_burst ab ON ab.id = rel.animal_burst_id
            JOIN pgtraj p ON p.id = ab.pgtraj_id
            WHERE p.pgtraj_name = '",pgtraj,"'
            )
        -- compute abs_angle, dist, dx, dy, dt, x, y
        SELECT
            t.r_rowname,
            ST_x(t.relocation1_geom) AS x, 
            ST_y(t.relocation1_geom) AS y,
            t.relocation_time AS date,
            t.dx,
            t.dy,
            t.dist,
            extract(epoch FROM t.dt) AS dt,
            t.r2n,
            CASE
                WHEN t.dist < 1e-07 THEN NULL
                WHEN t.dist >= 1e-07 THEN atan2(t.dy, t.dx)
            END AS abs_angle,
            t.rel_angle,
            t.animal_name AS id,
            t.burst_name AS burst,
            t.pgtraj_name AS pgtraj
        FROM (SELECT
                  relocation_time,
                  dt,
                  r_rowname,
                  relocation1_geom,
                  relocation2_geom,
                  burst_name,
                  animal_name,
                  pgtraj_name,
                  ST_length(step_geom) AS dist,
                  ST_X(relocation2_geom) - ST_X(relocation1_geom) AS dx,
                  ST_Y(relocation2_geom) - ST_Y(relocation1_geom) AS dy,
                  r2n,
                  rel_angle
              FROM step_geom
             ) AS t
        ORDER BY t.burst_name, t.relocation_time;")
    
        create_sql_query <- gsub(pattern = '\\s', replacement = " ",
                x = sql_query)
        
        res <- tryCatch({
                    
                    invisible(dbSendQuery(conn, create_sql_query))
                    message(paste0("View '", pgtraj,
                                    "_parameters' created in schema '",
                                    schema, "'."))
                    return(TRUE)
                    
                }, warning = function(war) {
                    
                    message(paste0("WARNING in creating view '",
                                    pgtraj,"_parameters' :"))
                    message(war)
                    return(war)
                    
                }, error = function(err) {
                    
                    message(paste0("ERROR in creating view '",
                                    pgtraj,"_parameters' :"))
                    message(err)
                    return(err)
                    
                })
    }
    
    sql_query <- paste0("SET search_path TO ", current_search_path, ";")
    invisible(dbSendQuery(conn, sql_query))
    
    return(res)
    
}


#' Creates a view of the step geometries for visualization.
#' 
#' @author Balázs Dukai
#' 
#' @param conn Connection object created with RPostgreSQL
#' @param schema String. Name of the schema that stores or will store the pgtraj data model.
#' 
#' @return TRUE on success, otherwise warning/error
#' 
###############################################################################
pgTrajViewStepGeom <- function(conn, schema, pgtraj) {
    
    current_search_path <- dbGetQuery(conn, "SHOW search_path;")
    sql_query <- paste0("SET search_path TO ", schema, ",public;")
    invisible(dbGetQuery(conn, sql_query))
    
    sql_query <- paste0(
    "CREATE OR REPLACE VIEW ",pgtraj,"_step_geometry AS
    SELECT
        s.id AS step_id,
        ST_Makeline(r1.geom, r2.geom) AS step_geom,
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
    JOIN s_i_b_rel rel ON rel.step_id = s.id
    JOIN animal_burst ab ON ab.id = rel.animal_burst_id
    JOIN pgtraj p ON p.id = ab.pgtraj_id
    WHERE p.pgtraj_name = '",pgtraj,"';"
    )
    create_sql_query <- gsub(pattern = '\\s', replacement = " ", x = sql_query)
    
    res <- tryCatch({
                
                invisible(dbSendQuery(conn, create_sql_query))
                message(paste0("View '",pgtraj,"_step_geometry' created in schema '", schema, "'."))
                return(TRUE)
                
            }, warning = function(war) {
                
                message(paste0("WARNING in creating view '",pgtraj,"_step_geometry' :"))
                message(war)
                return(war)
                
            }, error = function(err) {
                
                message(paste0("ERROR in creating view '",pgtraj,"_step_geometry' :"))
                message(err)
                return(err)
                
            })
    
    sql_query <- paste0("SET search_path TO ", current_search_path, ";")
    invisible(dbSendQuery(conn, sql_query))
    
    return(res)
}

