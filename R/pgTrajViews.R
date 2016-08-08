#' Computes the trajectory parameters (as in ltraj) for a pgtraj and creates a 
#' view for the pgtraj. The views are always named as '<pgtraj>_params'.
#' 
#' @author Balázs Dukai
#' 
#' @param conn Connection object created with RPostgreSQL
#' @param schema String. Name of the schema that stores or will store the pgtraj data model.
#' @param pgtraj String. Name of the pgtraj.
#' @param epsg Numeric. EPSG code of the relocation geometry.
#' 
#' @return TRUE on success, otherwise warning/error
#' 
###############################################################################
pgTrajViewParams <- function(conn, schema, pgtraj, epsg) {
    
    current_search_path <- dbGetQuery(conn, "SHOW search_path;")
    query <- paste0("SET search_path TO ", schema, ",public;")
    invisible(dbGetQuery(conn, query))
    
    query <- paste0(
    "CREATE OR REPLACE VIEW ",pgtraj,"_params AS
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
        )
    SELECT
        t.r_rowname,
        t.x,
        t.y,
        t.relocation_time AS date,
        t.dx,
        t.dy,
        t.dist,
        t.dt,
        t.r2n,
        atan2(t.dy, t.dx) AS abs_angle,
        CASE
            WHEN t.rel_angle <= -pi() THEN 2 * pi() + t.rel_angle
            WHEN t.rel_angle > pi() THEN t.rel_angle - 2 * pi()
            ELSE t.rel_angle
        END AS rel_angle,
        t.animal_name,
        t.burst_name AS burst,
        t.pgtraj_name AS pgtraj
    FROM (
        SELECT 
            s.r_rowname,
            ST_x(s.relocation1_geom) AS x, 
            ST_y(s.relocation1_geom) AS y,
            s.relocation_time,
            ST_X(s.relocation2_geom) - ST_X(s.relocation1_geom) AS dx,
            ST_Y(s.relocation2_geom) - ST_Y(s.relocation1_geom) AS dy,
            ST_length(s.step_geom) AS dist,
            extract(epoch FROM s.dt) AS dt,
            ST_Distance(startp.relocation1_geom, s.relocation1_geom)^2 AS r2n,
            r_angle.rel_angle,
            s.animal_name,
            s.burst_name,
            s.pgtraj_name,
            s.step_id
        FROM step_geom s
        JOIN (
            SELECT
                m.*,
                s.relocation1_geom,
                s.ab_id
            FROM (
                SELECT
                    min(s.step_id) AS first_step_id
                FROM step_geom s
                WHERE s.pgtraj_name = '",pgtraj,"'
                ) AS m
            JOIN step_geom s ON s.step_id = m.first_step_id
            ) AS startp
        ON startp.ab_id = s.ab_id
        LEFT JOIN (
            SELECT
                s2.step_id,
                s.r_rowname,
                (
                ST_Azimuth(s.relocation1_geom, s.relocation2_geom) -
                ST_Azimuth(s2.relocation1_geom, s2.relocation2_geom)
                ) AS rel_angle,
                s.relocation_time,
                s.ab_id
            FROM step_geom s
            LEFT JOIN LATERAL (SELECT *
                               FROM step_geom c
                               WHERE s.step_id < c.step_id
                               AND s.burst_name = c.burst_name
                               LIMIT 1) s2
            ON TRUE
            WHERE s.pgtraj_name = '",pgtraj,"'
            ) AS r_angle
        ON r_angle.step_id = s.step_id
    ) AS t
    ORDER BY t.burst_name, t.relocation_time;"
    )
    create_query <- gsub(pattern = '\\s', replacement = " ", x = query)
    
    res <- tryCatch({
                
                invisible(dbSendQuery(conn, create_query))
                message(paste0("View '", pgtraj, "_params' created in schema '", schema, "'."))
                return(TRUE)
                
            }, warning = function(war) {
                
                message(paste0("WARNING in creating view '",pgtraj,"_params' :"))
                message(war)
                return(war)
                
            }, error = function(err) {
                
                message(paste0("ERROR in creating view '",pgtraj,"_params' :"))
                message(err)
                return(err)
                
            })
    
    query <- paste0("SET search_path TO ", current_search_path, ";")
    invisible(dbSendQuery(conn, query))
    
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
pgTrajViewStepGeom <- function(conn, schema) {
    
    current_search_path <- dbGetQuery(conn, "SHOW search_path;")
    query <- paste0("SET search_path TO ", schema, ",public;")
    invisible(dbGetQuery(conn, query))
    
    query <- paste0(
    "CREATE OR REPLACE VIEW step_geom AS
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
    JOIN relocation r2 ON s.relocation_id_2 = r2.id
    JOIN s_i_b_rel rel ON rel.step_id = s.id
    JOIN animal_burst ab ON ab.id = rel.animal_burst_id
    JOIN pgtraj p ON p.id = ab.pgtraj_id;"
    )
    create_query <- gsub(pattern = '\\s', replacement = " ", x = query)
    
    res <- tryCatch({
                
                invisible(dbSendQuery(conn, create_query))
                message(paste0("View 'step_geom' created in schema '", schema, "'."))
                return(TRUE)
                
            }, warning = function(war) {
                
                message(paste0("WARNING in creating view 'step_geom' :"))
                message(war)
                return(war)
                
            }, error = function(err) {
                
                message(paste0("ERROR in creating view 'step_geom' :"))
                message(err)
                return(err)
                
            })
    
    query <- paste0("SET search_path TO ", current_search_path, ";")
    invisible(dbSendQuery(conn, query))
    
    return(res)
}

