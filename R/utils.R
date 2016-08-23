## ld_opt

##' Quick Conversion of Objects of Class ltraj from and to Dataframes
##'
##' Faster versions of \code{\link[adehabitatLT]{ld}} and
##' \code{\link[adehabitatLT]{dl}}.
##'
##' @param ltraj An object of class \code{ltraj}.
##' @return \code{ld_opt} returns a data frame with all trajectory
##'     parameters (and infolocs fields) as columns; \code{dl_opt}
##'     returns an object of class \code{ltraj}.
##' @keywords internal
##' @seealso See \code{\link[adehabitatLT]{ld}} for further details on
##'     the function and all available arguments.
##' @author Modified by Mathieu Basille \email{basille@@ufl.edu},
##'     Balázs Dukai \email{balazs.dukai@@gmail.com}

ld_opt <- function(ltraj) {
    if (!inherits(ltraj, "ltraj"))
        stop("ltraj should be of class ltraj")
    ## Equivalent of hab::ld(strict = FALSE)
    inf <- infolocs(ltraj)
    df <- data.frame(
        x = unlist(lapply(ltraj, function(x) x$x)),
        y = unlist(lapply(ltraj, function(x) x$y)),
        date = unlist(lapply(ltraj, function(x) x$date)),
        dx = unlist(lapply(ltraj, function(x) x$dx)),
        dy = unlist(lapply(ltraj, function(x) x$dy)),
        dist = unlist(lapply(ltraj, function(x) x$dist)),
        dt = unlist(lapply(ltraj, function(x) x$dt)),
        R2n = unlist(lapply(ltraj, function(x) x$R2n)),
        abs.angle = unlist(lapply(ltraj, function(x) x$abs.angle)),
        rel.angle = unlist(lapply(ltraj, function(x) x$rel.angle)),
        id = rep(id(ltraj), sapply(ltraj, nrow)),
        burst = rep(burst(ltraj), sapply(ltraj, nrow)),
        ## + Add row names as a column
        r.row.names = unlist(lapply(ltraj, function(x) rownames(x))))
    class(df$date) <- c("POSIXct", "POSIXt")
    attr(df$date, "tzone") <- attr(ltraj[[1]]$date, "tzone")
    if (!is.null(inf)) {
        nc <- ncol(inf[[1]])
        infdf <- as.data.frame(matrix(nrow = nrow(df), ncol = nc))
        names(infdf) <- names(inf[[1]])
        for (i in 1:nc)
            infdf[[i]] <- unlist(lapply(inf, function(x) x[[i]]))
        df <- cbind(df, infdf)
    }
    return(df)
}


## dl

##' @rdname ld_opt

dl_opt <- function(x, rnames = TRUE) {
    if (!inherits(x, "data.frame"))
        stop("x should be of class data.frame")
    ## Equivalent of hab::dl(strict = FALSE)
    trajnam <- c("x", "y", "date", "dx", "dy", "dist", "dt",
        "R2n", "abs.angle", "rel.angle")
    idd <- tapply(as.character(x$id), x$burst, unique)
    traj <- split(x[, names(x) %in% trajnam], x$burst)
    ## + Split row names by burst
    if (rnames) {
        traj_rname <- split(x[, "r.row.names"], x$burst)
            names(traj) <- NULL
        class(traj) <- c("ltraj", "list")
        attr(traj, "typeII") <- TRUE
        attr(traj, "regular") <- is.regular(traj)
        ## + Add r.row.names
        if (any(!(names(x) %in% c(trajnam, "id", "burst", "r.row.names")))) {
            inf <- split(x[, !(names(x) %in% c(trajnam, "id", "burst",
                "r.row.names")), drop = FALSE], x$burst)
            for (i in (1:length(traj))) {
                attr(traj[[i]], "id") <- as.character(idd[i])
                attr(traj[[i]], "burst") <- names(idd[i])
                attr(traj[[i]], "infolocs") <- inf[[i]]
                ## + Add r.row.names
                rownames(traj[[i]]) <- traj_rname[[i]]
            }
        } else for (i in (1:length(traj))) {
            attr(traj[[i]], "id") <- as.character(idd[i])
            attr(traj[[i]], "burst") <- names(idd[i])
            ## + Add r.row.names
            rownames(traj[[i]]) <- traj_rname[[i]]
        }
        return(traj)
    } else {
        names(traj) <- NULL
        class(traj) <- c("ltraj", "list")
        attr(traj, "typeII") <- TRUE
        attr(traj, "regular") <- is.regular(traj)
        if (any(!(names(x) %in% c(trajnam, "id", "burst")))) {
            inf <- split(x[, !(names(x) %in% c(trajnam, "id", "burst")),
                            drop = FALSE], x$burst)
            for (i in (1:length(traj))) {
                attr(traj[[i]], "id") <- as.character(idd[i])
                attr(traj[[i]], "burst") <- names(idd[i])
                attr(traj[[i]], "infolocs") <- inf[[i]]
                attr(traj[[i]], "row.names") <- rownames(traj[[i]])
            }
        } else for (i in (1:length(traj))) {
            attr(traj[[i]], "id") <- as.character(idd[i])
            attr(traj[[i]], "burst") <- names(idd[i])
            attr(traj[[i]], "row.names") <- rownames(traj[[i]])
        }
        return(traj)
    }

}


# pgTrajDB2TempT

#' Insert relocations from a source table into the table 'zgaqtsn_temp'. 
#' 
#' If relocations are given as X,Y coordinates, they are converted into a POINT 
#' geometry in PostGIS.
#' 
#' @author Balázs Dukai
#' 
#' @param conn Connection object created with RPostgreSQL
#' @param schema String. Name of the schema that stores or will store the pgtraj data model.
#' @param relocations_table String. Name of the table that stores the relocations, e.g. "public.relocations"
#' @param pgtrajs String. Name of the pgtraj or name of the field that stores the pgtraj names.
#' @param animals String. Name of the animal or name of the field that stores the animal names.
#' @param bursts String. Name of the burst or name of the field that stores the burst names.
#' @param timestamps String. Name of the field in relocations_table that contains the timestamps.
#' @param rids String. Name of the field in relocations_table that contains the numeric IDs of relocations.
#' @param relocations Vector of string(s). Name of the field(s) that contains 
#' the relocations in relocations_table. If relocations are stored as pairs of (X,Y) or 
#' (long, lat) coorindates, the coordinates should be separeted in two fields 
#' and referenced accordingly.
#' @param srid Numeric. The PostGIS SRID of the CRS of 'relocations'.
###############################################################################
pgTrajDB2TempT <- function(conn, schema, relocations_table, pgtrajs, animals,
        bursts = NULL, relocations, timestamps, rids, srid, proj4string,
        note, time_zone) {
    
    # Test for correct inputs
    test_input(pgtrajs, animals, relocations, bursts)
    
    # Set DB search path for the schema
    current_search_path <- dbGetQuery(conn, "SHOW search_path;")
    sql_query <- paste0("SET search_path TO ", schema, ",public;")
    invisible(dbSendQuery(conn, sql_query))
    
    # Table name is separated from schema declaration
    rd_split <- unlist(strsplit(relocations_table, "[.]"))
    
    # Populate 'zgaqtsn_temp'-------------------------------------------------
    # Insert relocations if trajectory Type I
    if (is.null(timestamps)) {
        # Relocations provided as point geometry
        if (length(relocations) == 1) {
            
            sql_query <- paste0("INSERT INTO zgaqtsn_temp (id, geom)
                            SELECT ",rids,",",relocations,"::geometry
                            FROM ",relocations_table,"
                            ORDER BY ",rids,";")
            sql_query <- gsub(pattern = '\\s', replacement = " ", x = sql_query)
            t <- c(t, dbSendQuery(conn, sql_query))
            
        } else if (length(relocations) == 2) {
            
            # Relocations provided as a coordinate pair
            x <- relocations[1]
            y <- relocations[2]
            sql_query <- paste0("INSERT INTO zgaqtsn_temp (id, geom)
                            SELECT ",rids,", ST_SetSRID(ST_MakePoint(",x,", ",y,"), ",srid,")
                            FROM ",relocations_table,"
                            ORDER BY ",rids,";")
            sql_query <- gsub(pattern = '\\s', replacement = " ", x = sql_query)
            invisible(dbSendQuery(conn, sql_query))
            
        }
    # If trajectory Type II
    } else {
        
        if (length(relocations) == 1) {
            
            # Relocations provided as point geometry
            sql_query <- paste0("INSERT INTO zgaqtsn_temp (id, geom, relocation_time)
                            SELECT ",rids,",",relocations,"::geometry, ",timestamps,"
                            FROM ",relocations_table,"
                            ORDER BY ",timestamps,";")
            sql_query <- gsub(pattern = '\\s', replacement = " ", x = sql_query)
            invisible(dbSendQuery(conn, sql_query))
            
        } else if (length(relocations) == 2) {
            
            # relocations provided as a coordinate pair
            x <- relocations[1]
            y <- relocations[2]
            sql_query <- paste0("INSERT INTO zgaqtsn_temp (id, geom, relocation_time)
                            SELECT ",rids,", ST_SetSRID(ST_MakePoint(",x,", ",y,"), ",srid,"), ",timestamps,"
                            FROM ",relocations_table,"
                            ORDER BY ",timestamps,";")
            sql_query <- gsub(pattern = '\\s', replacement = " ", x = sql_query)
            invisible(dbSendQuery(conn, sql_query))
        }
        
    }
    
    fields <- dbListFields(conn, rd_split)
    # Insert pgtraj
    if (pgtrajs %in% fields) {
        
        # use the field values for pgtraj
        sql_query <- paste0("UPDATE zgaqtsn_temp
                        SET pgtraj_name = a.",pgtrajs,"
                        FROM (
                        SELECT ",rids,", ",pgtrajs,"
                        FROM ",relocations_table,"
                        ) a
                        WHERE zgaqtsn_temp.id = a.",rids,";")
        sql_query <- gsub(pattern = '\\s', replacement = " ", x = sql_query)
        invisible(dbSendQuery(conn, sql_query))
        
    } else {
        
        # Use the string
        sql_query <- paste0("UPDATE zgaqtsn_temp SET pgtraj_name = '", pgtrajs, "';")
        invisible(dbSendQuery(conn, sql_query))
        
    }
    
    # Insert animal
    if (animals %in% fields) {
        
        # Use the field values for animal
        sql_query <- paste0("UPDATE zgaqtsn_temp
                        SET animal_name = a.",animals,"
                        FROM (
                        SELECT ",rids,", ",animals,"
                        FROM ",relocations_table,"
                        ) a
                        WHERE zgaqtsn_temp.id = a.",rids,";")
        sql_query <- gsub(pattern = '\\s', replacement = " ", x = sql_query)
        invisible(dbSendQuery(conn, sql_query))
        
    } else {
        
        # Use the string
        sql_query <- paste("UPDATE zgaqtsn_temp SET animal_name = '", animals, "';")
        invisible(dbSendQuery(conn, sql_query))
        
    }
    
    # Insert burst
     if (is_blank(bursts) & (animals %in% fields)) {
        
        # Use animal name as default burst name
        sql_query <- paste0("UPDATE zgaqtsn_temp
                        SET burst_name = a.",animals,"
                        FROM (
                        SELECT ",rids,", ",animals,"
                        FROM ",relocations_table,"
                        ) a
                        WHERE zgaqtsn_temp.id = a.",rids,";")
        sql_query <- gsub(pattern = '\\s', replacement = " ", x = sql_query)
        invisible(dbSendQuery(conn, sql_query))
        
    } else if (is_blank(bursts) & length(animals) == 1) {
        
        sql_query <- paste0("UPDATE zgaqtsn_temp SET burst_name = '",animals,"';")
        invisible(dbSendQuery(conn, sql_query))
        
    } else if (bursts %in% fields) {
        
        # Use the field values for bursts
        sql_query <- paste0("UPDATE zgaqtsn_temp
                        SET burst_name = a.",bursts,"
                        FROM (
                        SELECT ",rids,", ",bursts,"
                        FROM ",relocations_table,"
                        ) a
                        WHERE zgaqtsn_temp.id = a.",rids,";")
        sql_query <- gsub(pattern = '\\s', replacement = " ", x = sql_query)
        invisible(dbSendQuery(conn, sql_query))
        
    } else {
        
        # Use the string
        sql_query <- paste("UPDATE zgaqtsn_temp SET burst_name = '", bursts, "';")
        invisible(dbSendQuery(conn, sql_query))
        
    }
    
    # Insert note
    if (is_blank(note)) {
        
        # Set to NULL
        sql_query <- paste0("UPDATE zgaqtsn_temp SET note = NULL;")
        invisible(dbSendQuery(conn, sql_query))
        
    } else if (note %in% fields) {
        
        # use the values for note
        sql_query <- paste0("UPDATE zgaqtsn_temp
                        SET note = a.",note,"
                        FROM (
                        SELECT ",rids,", ",note,"
                        FROM ",relocations_table,"
                        ) a
                        WHERE zgaqtsn_temp.id = a.",rids,";")
        sql_query <- gsub(pattern = '\\s', replacement = " ", x = sql_query)
        invisible(dbSendQuery(conn, sql_query))
        
    } else {
        
        # Use the string
        sql_query <- paste0("UPDATE zgaqtsn_temp SET note = '", note, "';")
        invisible(dbSendQuery(conn, sql_query))
        
    }
    
    # Insert proj4string and time zone
    sql_query <- paste0("UPDATE zgaqtsn_temp
                         SET proj4string = '",proj4string,"',
                             time_zone = '",time_zone,"';")
    invisible(dbSendQuery(conn, sql_query))
    
    # Reset DB search path to the public schema
    sql_query <- paste0("SET search_path TO ", current_search_path, ";")
    invisible(dbSendQuery(conn, sql_query))
    
    return(TRUE)
}

# pgTrajTempT

#' Creates a temporary table in the 'traj' schema.
#' 
#' @description
#' Used by \code{pgTrajDB2TempT} and \code{pgTrajR2TempT} to create a temporary
#' table which will be populated by these functions. The temporary table's
#' name is a random string to avoid collation with user generated tables.
#' 
#' @author Balázs Dukai \email{balazs.dukai@@gmail.com}
#' 
#' @param conn Connection object created with RPostgreSQL
#' @param schema String. Name of the schema that stores or will store the pgtraj data model.
#' 
#' @return TRUE on success, otherwise warning/error
#' 
#' @examples
#' \dontrun{pgTrajTempT(conn, "traj_1")}
#' 
###############################################################################
pgTrajTempT <- function(conn, schema) {
    # Check if table already exists
    sql_query <- paste0("SELECT * FROM pg_tables WHERE schemaname = '", schema, "';")
    tables <- invisible(dbGetQuery(conn, sql_query))
    if ('zgaqtsn_temp' %in% tables$tablename) {
        acr <- NA
        while(is.na(acr) | !(acr %in% "y" | acr %in% "n")) {
            acr <- readline(paste("A table named 'zgaqtsn_temp' already exists in the schema", schema,
                            ". Do you want to delete it? [y/n]"))
            acr <- ifelse(grepl("y|n", acr), acr, as.character(acr))
        }
        if (acr %in% "n") {
            return(FALSE)
        } else {
            sql_query <- paste0("DROP TABLE IF EXISTS ", schema, ".zgaqtsn_temp;")
            invisible(dbSendQuery(conn, sql_query))
        }
    }
    
    # Create 'zgaqtsn_temp' table
    sql_query <- paste0("CREATE TEMPORARY TABLE zgaqtsn_temp (
                    id               serial,
                    pkey             text,
                    geom             geometry,
                    relocation_time  timestamptz,
                    burst_name       text,
                    animal_name      text,
                    pgtraj_name      text,
                    proj4string      text,
                    time_zone        text,
                    note             text
                    ) ON COMMIT DROP;")
    create_sql_query <- gsub(pattern = '\\s', replacement = " ", x = sql_query)
    
    res <- tryCatch({
        
        invisible(dbSendQuery(conn, create_sql_query))
        return(TRUE)
        
    }, warning = function(war) {
        
        message("WARNING in creating the temporary table:")
        message(war)
        return(war)
        
    }, error = function(err) {
        
        message("ERROR in creating the temporary table:")
        message(err)
        return(err)
        
    })
    
    return(res)
    
}


# pgTrajViewParams

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
                s.r_rowname,
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
            t.animal_name,
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


# pgTrajViewStepGeom

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



## test_input

##' Test inputs for the functions pgTrajDB2TempT()
##'
##' @param pgtrajs String. Name of the pgtraj or name of the field that
##'     stores the pgtraj names.
##' @param animals String. Name of the animal or name of the field that
##'     stores the animal names.
##' @param relocations String. Name of the field that contains the
##'     relocations in relocations_table.
##' @param bursts String. Name of the burst or name of the field that
##'     stores the burst names.
##' @param rids String. Name of the field in relocations_table that
##'     contains the numeric IDs of relocations.
##' @param epsg Numeric. The EPSG code of the Coordinate Reference
##'     System of the relocation coordinates in the ltraj. Defaults to
##'     0.
##' @return nothing
##' @keywords internal
##' @author Balázs Dukai \email{balazs.dukai@@gmail.com}

test_input <- function(pgtrajs = NULL, animals = NULL, relocations = NULL,
    bursts = NULL, rids = NULL, epsg = NULL) {
    test_pa <- function(x) {
        testthat::test_that("arguments 'pgtrajs', 'animals' and 'rids' have correct inputs",
            {
                testthat::expect_that(length(x) == 1, testthat::is_true())
                testthat::expect_that(all(!is.na(x)), testthat::is_true())
                testthat::expect_that(all(is.character(x)), testthat::is_true())
            })
    }
    if (!is.null(pgtrajs)) {
        test_pa(pgtrajs)
    }
    if (!is.null(animals)) {
        test_pa(animals)
    }
    if (!is.null(rids)) {
        test_pa(rids)
    }
    if (!is.null(bursts)) {
        testthat::test_that("argument 'bursts' has a correct input",
            {
                testthat::expect_that(length(bursts) == 1, testthat::is_true())
            })
    }
    if (!is.null(relocations)) {
        testthat::test_that("argument 'relocations' has a correct input",
            {
                testthat::expect_that(all(!is.na(relocations)),
                  testthat::is_true())
                testthat::expect_that(length(relocations) >=
                  1, testthat::is_true())
                testthat::expect_that(length(relocations) <=
                  2, testthat::is_true())
                testthat::expect_that(all(is.character(relocations)),
                  testthat::is_true())
            })
    }
    if (!is.null(epsg)) {
        testthat::test_that("argument 'epsg' has a correct input",
            {
                testthat::expect_that(is.numeric(epsg), testthat::is_true())
                testthat::expect_that(epsg%%1 == 0, testthat::is_true())
            })
    }
}


## is_blank

##' Test if an argument is either NA, NULL, NaN or empty string.
##'
##' @param x Any object.
##' @return boolean
##' @keywords internal
##' @author Balázs Dukai \email{balazs.dukai@@gmail.com}

is_blank <- function(x, false_triggers=FALSE){
    if(is.function(x)) return(FALSE) 
    return(
        is.null(x) ||
        length(x) == 0 ||
        all(is.na(x)) ||
        all(x=="") ||
        (false_triggers && all(!x))
    )
}




