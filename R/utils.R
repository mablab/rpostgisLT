# ld_opt

#' Quick Conversion of Objects of Class ltraj from and to data frames
#'
#' Faster versions of \code{\link[adehabitatLT]{ld}} and
#' \code{\link[adehabitatLT]{dl}}.
#'
#' @param ltraj An object of class \code{ltraj}.
#' @param x A data frame
#' @param rnames Whether to preserve row names from the data frame to the 
#'    ltraj
#' @return \code{ld_opt} returns a data frame with all trajectory 
#'    parameters as columns; \code{dl_opt} returns an object of class 
#'    \code{ltraj}.
#' @keywords internal
#' @seealso See \code{\link[adehabitatLT]{ld}} for further details on the 
#'    function and all available arguments.
#' @author Modified by Mathieu Basille \email{basille@@ufl.edu},
#'     Balázs Dukai \email{balazs.dukai@@gmail.com}

ld_opt <- function(ltraj) {
    if (!inherits(ltraj, "ltraj"))
        stop("ltraj should be of class ltraj")
    ## Equivalent of hab::ld(strict = FALSE)
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
        r.row.names = unlist(lapply(ltraj, function(x) rownames(x)))
        )
    # set date column to POSIX with tz
    class(df$date) <- c("POSIXct", "POSIXt")
    attr(df$date, "tzone") <- attr(ltraj[[1]]$date, "tzone")
    # infolocs handled in separate function
    return(df)
}


# dl

#' @rdname ld_opt

dl_opt <- function(x, rnames = TRUE) {
    if (!inherits(x, "data.frame"))
        stop("x should be of class data.frame")
    ## Equivalent of hab::dl(strict = FALSE)
    trajnam <- c("x", "y", "date", "dx", "dy", "dist", "dt",
        "R2n", "abs.angle", "rel.angle")
    ## Check if type I/II
    if (class(x$date)[1] == "integer") {
        type2<-FALSE
      } else {
        type2<-TRUE
      }
    ## Order bursts by table order    
    x$burst<- factor(x$burst, levels=unique(x$burst))
    idd <- tapply(as.character(x$id), x$burst, unique)
    traj <- split(x[, names(x) %in% trajnam], x$burst)
    
    ## + Split row names by burst
    ## infolocs handled in seperate function
    if (rnames) {
        traj_rname <- split(x[, "r.row.names"], x$burst)
            names(traj) <- NULL
        class(traj) <- c("ltraj", "list")
        attr(traj, "typeII") <- type2
        attr(traj, "regular") <- is.regular(traj)
          for (i in (1:length(traj))) {
            attr(traj[[i]], "id") <- as.character(idd[i])
            attr(traj[[i]], "burst") <- names(idd[i])
            ## + Add r.row.names
            rownames(traj[[i]]) <- traj_rname[[i]]
        }
        return(traj)
    } else {
        names(traj) <- NULL
        class(traj) <- c("ltraj", "list")
        attr(traj, "typeII") <- type2
        attr(traj, "regular") <- is.regular(traj)
          for (i in (1:length(traj))) {
            attr(traj[[i]], "id") <- as.character(idd[i])
            attr(traj[[i]], "burst") <- names(idd[i])
            attr(traj[[i]], "row.names") <- rownames(traj[[i]])
        }
        return(traj)
    }
}


# pgTrajDB2TempT

#' Insert relocations from a source table into the table 'zgaqtsn_temp', 
#' used in asPgtraj 
#' 
#' If relocations are given as X,Y coordinates, they are converted into 
#' a POINT geometry in PostGIS.
#' 
#' @param conn Connection object created with RPostgreSQL
#' @param schema String. Name of the schema that stores or will store 
#'    the pgtraj data model.
#' @param relocations_table String. Name of the table that stores the 
#'    relocations, e.g. c("schema","relocations")
#' @param pgtrajs String. Name of the pgtraj or name of the field that 
#'    stores the pgtraj names.
#' @param animals String. Name of the animal or name of the field that 
#'    stores the animal names.
#' @param bursts String. Name of the burst or name of the field that 
#'    stores the burst names.
#' @param timestamps String. Name of the field in relocations_table 
#'    that contains the timestamps.
#' @param rids String. Name of the field in relocations_table that 
#'    contains the numeric IDs of relocations.
#' @param relocations Vector of string(s). Name of the field(s) that 
#'    contains the relocations in relocations_table. If relocations are 
#'    stored as pairs of (X,Y) or (long, lat) coordinates, the coordinates 
#'    should be separated in two fields and referenced accordingly.
#' @param srid Numeric. The PostGIS SRID of the CRS of 'relocations'.
#' @param proj4string String. The PROJ4 string to be inserted into 
#'    \code{pgtraj} table.
#' @param note String. Comment on the pgtraj. The comment is only used 
#'    in the database and not transferred into an ltraj.
#' @param clauses String. Additional SQL to modify select query from 
#'    relocations_table
#' @param time_zone String. Time zone to be inserted into \code{pgtraj} 
#'    table.
#' 
#' @author Balázs Dukai
#' @keywords internal

pgTrajDB2TempT <- function(conn, schema, relocations_table, pgtrajs, animals,
        bursts = NULL, relocations, timestamps, rids, srid, proj4string,
        note, clauses, time_zone) {
    # check table name
    relocations_table_q <- paste(dbTableNameFix(conn,relocations_table),
                                 collapse = ".")
    # sanitize schema, rids, relocations 
    schema_q <- dbQuoteIdentifier(conn,schema)
    rids_q<- dbQuoteIdentifier(conn,rids)
    relocations_q<- dbQuoteIdentifier(conn,relocations)

    # Test for correct inputs
    test_input(pgtrajs, animals, relocations, bursts)
    
    # Set DB search path for the schema
    current_search_path <- dbGetQuery(conn, "SHOW search_path;")
    sql_query <- paste0("SET search_path TO ", schema_q, ",public;")
    invisible(dbExecute(conn, sql_query))
    
    # Populate 'zgaqtsn_temp'
    # Insert relocations if trajectory Type I
    if (is.null(timestamps)) {
        # Relocations provided as point geometry
        if (length(relocations) == 1) {
            sql_query <- paste0("INSERT INTO zgaqtsn_temp (id, geom)
                            SELECT ",rids_q,",",relocations_q,"::geometry
                            FROM ",relocations_table_q," ",
                            clauses,
                            " ORDER BY ",rids_q,";")
            sql_query <- gsub(pattern = '\\s', replacement = " ", x = sql_query)
            t <- c(t, dbExecute(conn, sql_query))
            
        } else if (length(relocations) == 2) {
            # Relocations provided as a coordinate pair
            x <- relocations_q[1]
            y <- relocations_q[2]
            sql_query <- paste0("INSERT INTO zgaqtsn_temp (id, geom)
                            SELECT ",rids_q,", ST_SetSRID(ST_MakePoint(",x,", ",y,"), ",srid,")
                            FROM ",relocations_table_q," ",
                            clauses,
                            " ORDER BY ",rids_q,";")
            sql_query <- gsub(pattern = '\\s', replacement = " ", x = sql_query)
            invisible(dbExecute(conn, sql_query))
        }
    # If trajectory Type II
    } else {
        # sanitize timestamps
        timestamps_q<-dbQuoteIdentifier(conn,timestamps)
        
        if (length(relocations) == 1) {
            # Relocations provided as point geometry
            sql_query <- paste0("INSERT INTO zgaqtsn_temp (id, geom, relocation_time)
                            SELECT ",rids_q,",",relocations_q,"::geometry, ",timestamps_q,"
                             FROM ",relocations_table_q," ",
                             clauses,
                             " ORDER BY ",timestamps_q,";")
            sql_query <- gsub(pattern = '\\s', replacement = " ", x = sql_query)
            invisible(dbExecute(conn, sql_query))
        } else if (length(relocations) == 2) {
            # relocations provided as a coordinate pair
            x <- relocations_q[1]
            y <- relocations_q[2]
            sql_query <- paste0("INSERT INTO zgaqtsn_temp (id, geom, relocation_time)
                            SELECT ",rids_q,", ST_SetSRID(ST_MakePoint(",x,", ",y,"), ",srid,"), ",timestamps_q,"
                            FROM ",relocations_table_q," ",
                            clauses,
                            " ORDER BY ",timestamps_q,";")
            sql_query <- gsub(pattern = '\\s', replacement = " ", x = sql_query)
            invisible(dbExecute(conn, sql_query))
        }
    }
    # maybe analyze table here (to speed up updates)
    fields <- dbTableInfo(conn, relocations_table)$column_name
    # Insert pgtraj
    # sanitize pgtraj
    pgtrajs_q<-dbQuoteIdentifier(conn, pgtrajs)
    
    if (pgtrajs %in% fields) {
        # use the field values for pgtraj
        sql_query <- paste0("UPDATE zgaqtsn_temp
                        SET pgtraj_name = a.",pgtrajs_q,"
                        FROM (
                        SELECT ",rids_q,", ",pgtrajs_q,"
                        FROM ",relocations_table_q," ", clauses,"
                        ) a
                        WHERE zgaqtsn_temp.id = a.",rids_q,";")
        sql_query <- gsub(pattern = '\\s', replacement = " ", x = sql_query)
        invisible(dbExecute(conn, sql_query))
    } else {
        # use the string
        # only allow pgtraj names that begin with letters or numbers
        if (!grepl("^[0-9A-Za-z]", pgtrajs)) {
            stop("Invalid pgtraj name. Valid pgtraj names must begin with a letter or number.")
        }
        sql_query <- paste0("UPDATE zgaqtsn_temp SET pgtraj_name = ", dbQuoteString(conn,pgtrajs), ";")
        invisible(dbExecute(conn, sql_query))
    }
    
    # Insert animal
    # sanitizes animals
    animals_q<-dbQuoteIdentifier(conn,animals)
    if (animals %in% fields) {
        # Use the field values for animal
        sql_query <- paste0("UPDATE zgaqtsn_temp
                        SET animal_name = a.",animals_q,"
                        FROM (
                        SELECT ",rids_q,", ",animals_q,"
                        FROM ",relocations_table_q," ", clauses,"
                        ) a
                        WHERE zgaqtsn_temp.id = a.",rids_q,";")
        sql_query <- gsub(pattern = '\\s', replacement = " ", x = sql_query)
        invisible(dbExecute(conn, sql_query))
    } else {
        # Use the string
        sql_query <- paste("UPDATE zgaqtsn_temp SET animal_name = ",dbQuoteString(conn,animals), ";")
        invisible(dbExecute(conn, sql_query))
    }
    
    # Insert burst
     if (is_blank(bursts) & (animals %in% fields)) {
        # Use animal name as default burst name
        sql_query <- paste0("UPDATE zgaqtsn_temp
                        SET burst_name = a.",animals_q,"
                        FROM (
                        SELECT ",rids_q,", ",animals_q,"
                        FROM ",relocations_table_q," ", clauses,"
                        ) a
                        WHERE zgaqtsn_temp.id = a.",rids_q,";")
        sql_query <- gsub(pattern = '\\s', replacement = " ", x = sql_query)
        invisible(dbExecute(conn, sql_query))
    } else if (is_blank(bursts) & length(animals) == 1) {
        sql_query <- paste0("UPDATE zgaqtsn_temp SET burst_name = ",dbQuoteString(conn,animals),";")
        invisible(dbExecute(conn, sql_query))
    } else if (bursts %in% fields) {
        # sanitize bursts
        bursts_q<-dbQuoteIdentifier(conn,bursts)
        
        # Use the field values for bursts
        sql_query <- paste0("UPDATE zgaqtsn_temp
                        SET burst_name = a.",bursts_q,"
                        FROM (
                        SELECT ",rids_q,", ",bursts_q,"
                        FROM ",relocations_table_q," ", clauses,"
                        ) a
                        WHERE zgaqtsn_temp.id = a.",rids_q,";")
        sql_query <- gsub(pattern = '\\s', replacement = " ", x = sql_query)
        invisible(dbExecute(conn, sql_query))
    } else {
        # Use the string
        sql_query <- paste("UPDATE zgaqtsn_temp SET burst_name = ", dbQuoteString(conn,bursts), ";")
        invisible(dbExecute(conn, sql_query))
    }
    
    # Insert note
    if (is_blank(note)) {
        # Set to NULL
        sql_query <- paste0("UPDATE zgaqtsn_temp SET note = NULL;")
        invisible(dbExecute(conn, sql_query))
    } else if (note %in% fields) {
        note_q<-dbQuoteIdentifier(conn,note)
        # use the values for note
        sql_query <- paste0("UPDATE zgaqtsn_temp
                        SET note = a.",note_q,"
                        FROM (
                        SELECT ",rids_q,", ",note_q,"
                        FROM ",relocations_table_q," ", clauses,"
                        ) a
                        WHERE zgaqtsn_temp.id = a.",rids_q,";")
        sql_query <- gsub(pattern = '\\s', replacement = " ", x = sql_query)
        invisible(dbExecute(conn, sql_query))
    } else {
        # Use the string
        sql_query <- paste0("UPDATE zgaqtsn_temp SET note = ",dbQuoteString(conn,note),";")
        invisible(dbExecute(conn, sql_query))
    }
    
    # Insert proj4string and time zone
    if (!is_blank(proj4string)) {
     sql_query <- paste0("UPDATE zgaqtsn_temp
                          SET proj4string = ",dbQuoteString(conn, proj4string),";")
     invisible(dbExecute(conn, sql_query))
    }
    
    if (!is_blank(time_zone)) {
     sql_query <- paste0("UPDATE zgaqtsn_temp
                          SET time_zone = ",dbQuoteString(conn, time_zone),";")
     invisible(dbExecute(conn, sql_query))
    }
    
    # Reset DB search path to the public schema
    sql_query <- paste0("SET search_path TO ", current_search_path, ";")
    invisible(dbExecute(conn, sql_query))
    
    return(TRUE)
}

# pgTrajTempT

#' Creates a temporary table in the 'traj' schema.
#' 
#' @description
#' Used by \code{pgTrajDB2TempT} and \code{pgTrajR2TempT} to create a 
#' temporary table which will be populated by these functions. The 
#' temporary table's name is a random string to avoid collation with 
#' user generated tables.
#' 
#' @param conn Connection object created with RPostgreSQL
#' @param schema String. Name of the schema that stores or will store 
#'    the pgtraj data model
#' 
#' @return TRUE on success, otherwise warning/error
#' 
#' @author Balázs Dukai \email{balazs.dukai@@gmail.com}
#' @keywords internal
#' 
#' @examples
#' \dontrun{pgTrajTempT(conn, "traj_1")}
pgTrajTempT <- function(conn, schema) {
    # Check if table already exists
    sql_query <- paste0("SELECT * FROM pg_tables WHERE schemaname = ", dbQuoteString(conn,schema), ";")
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
            sql_query <- paste0("DROP TABLE IF EXISTS ", dbQuoteIdentifier(conn,schema), ".zgaqtsn_temp;")
            invisible(dbExecute(conn, sql_query))
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
        invisible(dbExecute(conn, create_sql_query))
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

#' Computes the trajectory parameters (as in ltraj) for a pgtraj and 
#' creates a view for the pgtraj. The views are always named as 
#' 'parameters_<pgtraj_name>'.
#' 
#' @author Balázs Dukai
#' 
#' @param conn Connection object created with RPostgreSQL
#' @param schema String. Name of the schema that stores or will store the 
#'    pgtraj data model.
#' @param pgtraj String. Name of the pgtraj.
#' @param epsg Numeric. EPSG code of the relocation geometry.
#' @param db Boolean. A switch that controls the parameters view creation 
#'    depending on source of data (R or PostgreSQL). If TRUE, raw data input 
#'    from a database table is assumed. In this case all parameters will be 
#'    computed. If FALSE, it is assumed that an ltraj was input from R with 
#'    already computed parameters. In this case R2n and rel.angle will not 
#'    be recomputed, but reused from the ltraj.
#' 
#' @return TRUE on success, otherwise warning/error
#' 
#' @keywords internal
#' 
pgTrajViewParams <- function(conn, schema, pgtraj, epsg, db = TRUE) {
    
    current_search_path <- dbGetQuery(conn, "SHOW search_path;")
    sql_query <- paste0("SET search_path TO ", dbQuoteIdentifier(conn,schema), ",public;")
    invisible(dbExecute(conn, sql_query))
    
    view <- dbQuoteIdentifier(conn,paste0("parameters_",pgtraj))
    
    if (db) {
        sql_query <- paste0(
        "CREATE OR REPLACE VIEW ",view," AS
        WITH step_geom AS (
            SELECT
                s.id AS step_id,
                ST_Makeline(r1.geom, r2.geom) AS step_geom,
                r1.relocation_time,
                s.dt,
                s.r_rowname,
                r1.geom AS relocation1_geom,
                r2.geom AS relocation2_geom,
                ab.id as burst_order,
                ab.burst_name,
                ab.animal_name,
                p.pgtraj_name,
                ab.id AS ab_id
            FROM step s
            JOIN relocation r1 ON s.relocation_id_1 = r1.id
            LEFT JOIN relocation r2 ON s.relocation_id_2 = r2.id
            JOIN s_b_rel rel ON rel.step_id = s.id
            JOIN animal_burst ab ON ab.id = rel.animal_burst_id
            JOIN pgtraj p ON p.id = ab.pgtraj_id
            WHERE p.pgtraj_name = ",dbQuoteString(conn,pgtraj),"
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
                t.burst_order,
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
                      burst_order,
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
                a.burst_order AS burst_order,
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
            p.step_id,
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
        ORDER BY p.burst_order, p.step_id;"
        )
        create_sql_query <- gsub(pattern = '\\s', replacement = " ",
                x = sql_query)
        
        res <- tryCatch({
                    invisible(dbExecute(conn, create_sql_query))
                    message(paste0("View 'parameters_",pgtraj,"' created in schema '",
                                    schema, "'."))
                    return(TRUE)
                }, warning = function(war) {
                    message(paste0("WARNING in creating view 'parameters_",pgtraj,"' :"))
                    message(war)
                    return(war)
                }, error = function(err) {
                    message(paste0("ERROR in creating view 'parameters_",pgtraj,"' :"))
                    message(err)
                    return(err)
                })
    } else {
        sql_query <- paste0(
        "CREATE OR REPLACE VIEW ",view," AS
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
                ab.id as burst_order,
                ab.animal_name,
                p.pgtraj_name,
                ab.id AS ab_id
            FROM step s
            JOIN relocation r1 ON s.relocation_id_1 = r1.id
            LEFT JOIN relocation r2 ON s.relocation_id_2 = r2.id
            JOIN s_b_rel rel ON rel.step_id = s.id
            JOIN animal_burst ab ON ab.id = rel.animal_burst_id
            JOIN pgtraj p ON p.id = ab.pgtraj_id
            WHERE p.pgtraj_name = ",dbQuoteString(conn,pgtraj),"
            )
        SELECT t.step_id,
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
        FROM (SELECT step_id,
                  relocation_time,
                  dt,
                  r_rowname,
                  relocation1_geom,
                  relocation2_geom,
                  burst_name,
                  burst_order,
                  animal_name,
                  pgtraj_name,
                  ST_length(step_geom) AS dist,
                  ST_X(relocation2_geom) - ST_X(relocation1_geom) AS dx,
                  ST_Y(relocation2_geom) - ST_Y(relocation1_geom) AS dy,
                  r2n,
                  rel_angle
              FROM step_geom
             ) AS t
        ORDER BY t.burst_order, t.step_id;")
    
        create_sql_query <- gsub(pattern = '\\s', replacement = " ",
                x = sql_query)
        
        res <- tryCatch({
                    invisible(dbExecute(conn, create_sql_query))
                    message(paste0("View 'parameters_",pgtraj,"' created in schema '",
                                    schema, "'."))
                    return(TRUE)
                }, warning = function(war) {
                    message(paste0("WARNING in creating view 'parameters_",pgtraj,"' :"))
                    message(war)
                    return(war)
                }, error = function(err) {
                    message(paste0("ERROR in creating view 'parameters_",pgtraj,"' :"))
                    message(err)
                    return(err)
                })
    }
    
    sql_query <- paste0("SET search_path TO ", current_search_path, ";")
    invisible(dbExecute(conn, sql_query))
    
    return(res)
}


# pgTrajViewStepGeom

#' Creates a view of the step geometries for visualization.
#' 
#' @param conn Connection object created with RPostgreSQL
#' @param schema String. Name of the schema that stores or will store the 
#'    pgtraj data model.
#' @param pgtraj String. Name of the pgtraj.
#' 
#' @return TRUE on success, otherwise warning/error
#' 
#' @author Balázs Dukai
#' @keywords internal
#' 

pgTrajViewStepGeom <- function(conn, schema, pgtraj) {
    
    current_search_path <- dbGetQuery(conn, "SHOW search_path;")
    sql_query <- paste0("SET search_path TO ", dbQuoteIdentifier(conn,schema), ",public;")
    invisible(dbExecute(conn, sql_query))
    
    sql_query <- paste0(
        "SELECT public.st_srid(r.geom)
        FROM relocation r
        JOIN step s ON s.relocation_id_1 = r.id
        JOIN s_b_rel rel ON rel.step_id = s.id
        JOIN animal_burst ab ON ab.id = rel.animal_burst_id
        JOIN pgtraj p ON p.id = ab.pgtraj_id
        WHERE p.pgtraj_name = ",dbQuoteString(conn,pgtraj),"
        AND r.geom NOTNULL
        LIMIT 1;")
    srid <- dbGetQuery(conn, sql_query)[1, 1]
    
    
    view <- dbQuoteIdentifier(conn,paste0("step_geometry_",pgtraj))
    
    sql_query <- paste0(
    "CREATE OR REPLACE VIEW ",view," AS
    SELECT
        s.id AS step_id,
        ST_Makeline(r1.geom, r2.geom)::geometry(LINESTRING,",srid,") AS step_geom,
        r1.relocation_time,
        s.dt,
        s.r_rowname,
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
    WHERE p.pgtraj_name = ",dbQuoteString(conn,pgtraj),"
    ORDER BY ab.id, s.id;"
    )
    create_sql_query <- gsub(pattern = '\\s', replacement = " ", x = sql_query)
    
    res <- tryCatch({
                invisible(dbExecute(conn, create_sql_query))
                message(paste0("View 'step_geometry_",pgtraj,"' created in schema '", schema, "'."))
                return(TRUE)
            }, warning = function(war) {
                message(paste0("WARNING in creating view 'step_geometry_",pgtraj,"' :"))
                message(war)
                return(war)
            }, error = function(err) {
                message(paste0("ERROR in creating view 'step_geometry_",pgtraj,"' :"))
                message(err)
                return(err)
            })
    
    sql_query <- paste0("SET search_path TO ", current_search_path, ";")
    invisible(dbExecute(conn, sql_query))
    
    return(res)
}

# test_input

#' Test inputs for the functions pgTrajDB2TempT()
#'
#' @param pgtrajs String. Name of the pgtraj or name of the field that
#'    stores the pgtraj names.
#' @param animals String. Name of the animal or name of the field that
#'    stores the animal names.
#' @param relocations String. Name of the field that contains the
#'    relocations in relocations_table.
#' @param bursts String. Name of the burst or name of the field that
#'    stores the burst names.
#' @param rids String. Name of the field in relocations_table that
#'    contains the numeric IDs of relocations.
#' @param epsg Numeric. The EPSG code of the Coordinate Reference
#'    System of the relocation coordinates in the ltraj. Defaults to 0.
#' @return nothing
#' 
#' @author Balázs Dukai \email{balazs.dukai@@gmail.com}
#' @keywords internal

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

# is_blank

#' Test if an argument is either NA, NULL, NaN or empty string.
#'
#' @param x Any object.
#' @return boolean
#' @author Balázs Dukai \email{balazs.dukai@@gmail.com}
#' @keywords internal

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


# writeInfoFromLtraj

#' Write infolocs table to database from ltraj in ltraj2pgtraj()
#'
#' @param conn A PostgreSQL connection object.
#' @param ltraj An adehabitatLT ltraj object
#' @param pgtraj String, name of the pgtraj being created
#' @param schema String, name of the schema holding the pgtraj
#' @return TRUE on successful infolocs writing
#' 
#' @author David Bucklin \email{dbucklin@@ufl.edu}
#' @keywords internal

writeInfoFromLtraj <- function(conn, ltraj, pgtraj, schema) {
    
    inf <- infolocs(ltraj)
    if (is.null(inf)) {
        #message("No Infolocs data in ltraj.")
        return(FALSE)
    }
    
    # table_name
    iloc_nm <- paste0("infolocs_", pgtraj)
    
    # query-safe identifier names
    iloc_nmq <- dbQuoteIdentifier(conn, iloc_nm)
    schemaq <- dbQuoteIdentifier(conn, schema)
    
    burst <- rep(burst(ltraj), sapply(ltraj, nrow))
    rnms <- unlist(lapply(ltraj, function(x) rownames(x)))
    
    # bind infolocs data frames
    icols <- unlist(lapply(inf, function(x) colnames(x)))
    icols <- unique(icols)

    for (l in 1:length(inf)) {
        missing <- icols[!icols %in% names(inf[[l]])]
        inf[[l]][missing] <- NA
    }
    
    # bind data frames
    iloc_df<-as.data.frame(inf[[1]][FALSE,])
    
    # assuming burst order always matches infolocs order
    bursts<-burst(ltraj)
    for (b in 1:length(inf)) {
      b_nm<-bursts[b]
      
      # get burst dataframe
      b_df<-inf[[b]]
      
      # only reserved name is 'step_id'
      if ("step_id" %in% names(b_df)) {
          fix_df <- data.frame(b_df[1, ], step_id = 1)
          fix_nm <- names(fix_df)[length(names(fix_df))]
          names(b_df)[names(b_df) == "step_id"] <- fix_nm
          #message("Reserved column name 'step_id' found in infolocs and changed to '", 
          #    fix_nm, "'.")
      }
      
      ##### infolocs definition section #####
      types <- unlist(lapply(b_df, function(x) {
          class(x)[1]
      }))
      
      # handle attribute (time zones)
      attr2 <- lapply(b_df[1, ], function(x) {
          attr(x, "tzone")[1]
      })
      badtz <- unlist(lapply(attr2, function(x) {
          any(is.null(x), !x %in% OlsonNames())
      }))
      attr2[badtz] <- "NULL"
      attr2 <- unlist(attr2)
      
      # convert non-matching tz time to db tz
      pgtz<-dbGetQuery(conn, "SHOW timezone;")[1,1]
      tzl<-names(attr2[attr2 != "NULL" & attr2 != pgtz])
      for (t in tzl) {
        eval(parse(
          text = paste0("attributes(b_df$",t,")$tzone <- pgtz") 
        ))
      }
      
      # handle attribute (factor levels)
      fact <- unlist(lapply(b_df[1, ], function(x) {
          paste0("/*/", paste(attr(x, "levels"), collapse = "/*/"), 
              "/*/")
      }))
      fact <- gsub(",", "\\,", fact, fixed = TRUE)
      attr2[!fact == "/*//*/"] <- fact[!fact == "/*//*/"]
      
      # make array of columns, types, and time zones
      info_nm <- paste0("'{{", paste(names(b_df), collapse = ","), 
          "},{", paste(as.character(types), collapse = ","), "},{", 
          paste(as.character(attr2), collapse = ","), "}}'")
      
      # write info_nm to animal_burst.info_cols
      sql_query <- paste0("UPDATE ", schemaq, ".animal_burst 
                          SET (info_cols) = (",info_nm, ")
                          FROM ", schemaq, ".pgtraj
                          WHERE pgtraj.id = animal_burst.pgtraj_id
                          AND pgtraj.pgtraj_name = ",dbQuoteString(conn, pgtraj),
                          " AND animal_burst.burst_name = ",dbQuoteString(conn,b_nm),";")
      dbExecute(conn, sql_query)
      
      # add to final data frame
      iloc_df<-rbind(iloc_df,b_df)
    }
    ##### end infolocs definition section #####
    
    # add original ltraj row and burst names, and step_id rowname
    # and burst are just for join, discarded later
    iloc_df$r_rowname931bqvz <- as.character(rnms)
    iloc_df$burst_931bqvz <- as.character(burst)
    iloc_df$step_id <- as.integer(1)
    
    # insert into new table (just columns)
    ctq<-dbBuildTableQuery(conn,c(schema, iloc_nm), iloc_df)
    invisible(dbExecute(conn, ctq))
    
    tztypes <- unlist(lapply(iloc_df, function(x) {
        class(x)[1]
    }))
    tz <- tztypes %in% c("POSIXct", "POSIXt")  #POSIXlt is stored as text
    for (n in names(iloc_df)[tz]) {
        dbExecute(conn, paste0("ALTER TABLE ", schemaq, ".", 
            iloc_nmq, " ALTER COLUMN ", dbQuoteIdentifier(conn, 
                n), " TYPE timestamp;"))
    }
    suppressMessages(pgInsert(conn, name = c(schema, iloc_nm), 
        data.obj = iloc_df, alter.names = FALSE))
    
    dbComment(conn,name = c(schema, iloc_nm),type = "table",
              comment = paste0("Infolocs (additional information on locations/steps) for the pgtraj ",
                               dbQuoteIdentifier(conn,pgtraj),"."), display = FALSE)
    
    # update step_id column
    sql_query<-paste0("UPDATE ",schemaq,".",iloc_nmq," a SET
                      step_id = b.step_id FROM 
                          (SELECT s_b_rel.step_id as step_id,
                            step.r_rowname as r_rowname931bqvz,
                            animal_burst.burst_name as burst_931bqvz
                          FROM ",
                            schemaq,".pgtraj, ",
                            schemaq,".animal_burst, ",
                            schemaq,".s_b_rel, ",
                            schemaq,".step 
                          WHERE
                            pgtraj.id = animal_burst.pgtraj_id AND
                            animal_burst.id = s_b_rel.animal_burst_id AND
                            s_b_rel.step_id = step.id AND
                            pgtraj_name = ",dbQuoteString(conn,pgtraj),"
                          ORDER BY step_id) b WHERE
                      a.r_rowname931bqvz = b.r_rowname931bqvz AND
                      a.burst_931bqvz = b.burst_931bqvz;")
    dbExecute(conn,sql_query)
  
    # drop r_rowname and burst columns
    dbColumn(conn, c(schema, iloc_nm), colname = "burst_931bqvz", 
        action = "drop", display = FALSE)
    dbColumn(conn, c(schema, iloc_nm), colname = "r_rowname931bqvz", 
        action = "drop", display = FALSE)
    
    # add primary key to step_id
    dbAddKey(conn, c(schema, iloc_nm), type = "primary", "step_id", 
        display = FALSE)
    
    # add foreign key to step_id
    sql_query <- paste0("ALTER TABLE ", schemaq, ".", iloc_nmq, 
        " ADD FOREIGN KEY (step_id) REFERENCES ", schemaq, ".step (id)
                    ON UPDATE NO ACTION ON DELETE CASCADE;")
    dbExecute(conn, sql_query)
    message(paste0("Infolocs for pgtraj '", pgtraj, "' written to table '", 
        iloc_nm, "'."))
    return(TRUE)
}

# writeInfoFromDB

#' Write infolocs table to database from database in pgtraj2ltraj()
#' 
#' @param conn A PostgreSQL connection object.
#' @param pgtraj String, name of the pgtraj
#' @param schema String, name of the schema holding the pgtraj
#' @param info_cols String. Optional character vector of column names of 
#'    additional information on relocations (replicating "infolocs" from the
#'    \code{adehabitatLT} object \code{ltraj}).
#' @param info_table Character vector of \code{c("schema","table")} holding the 
#'    \code{info_cols}.
#' @param info_rids String. Column name of unique integer ID in \code{info_table} 
#'    to join with \code{rids} of the original relocations.
#' @return TRUE on successful infolocs writing
#' @author David Bucklin \email{dbucklin@@ufl.edu}
#' @keywords internal


writeInfoFromDB <- function(conn, pgtraj, schema, info_cols, 
    info_table, info_rids) {
    
    # data goes with rids to zqaqtsn_temp as id; send this id as
    # original relocation ids (orig_id) to relocation table
    # select orig_id using join step.relocation_id_1 =
    # relocation.id where steps in current pgtraj
  
    # check if cols in table
    tabl_chk<-rpostgis::dbTableInfo(conn,info_table)$column_name
    info_cols<-info_cols[info_cols %in% tabl_chk]
  
    # only reserved name is 'step_id'
    fix_df <- make.names(c("step_id",info_cols), unique = TRUE)
    fix_df <- fix_df[2:length(fix_df)]
    info_repl<-info_cols
    info_repl[info_cols=="step_id"]<-fix_df[info_cols=="step_id"]
    info_replq<-dbQuoteIdentifier(conn,info_repl)
    
    # query-safe identifier names
    info_tableq <- dbQuoteIdentifier(conn, info_table)
    info_colsq <- dbQuoteIdentifier(conn, info_cols)
    schemaq <- dbQuoteIdentifier(conn, schema)
    
    ins_cols <- paste("info_tab.", info_colsq, " AS ", info_replq, sep = "")
    info_ridsq <- dbQuoteIdentifier(conn, info_rids)
    iloc_nm <- paste0("infolocs_", pgtraj)
    iloc_nmq <- dbQuoteIdentifier(conn, iloc_nm)
  
    # create and populate table
    sql_query<-paste0("SELECT step.id as step_id,",
                paste(ins_cols,collapse = ",")," 
              INTO TABLE ",schemaq,".",iloc_nmq," FROM ",
                paste(info_tableq,collapse=".")," as info_tab, ",
                schemaq,".relocation, ",
                schemaq,".pgtraj, ",
                schemaq,".animal_burst, ",
                schemaq,".s_b_rel, ",
                schemaq,".step
              WHERE
                pgtraj.id = animal_burst.pgtraj_id AND
                animal_burst.id = s_b_rel.animal_burst_id AND
                s_b_rel.step_id = step.id AND
                step.relocation_id_1 = relocation.id AND
                relocation.orig_id = info_tab.",info_ridsq," AND
                pgtraj_name = ",dbQuoteString(conn,pgtraj),"
              ORDER BY step_id;")
    dbExecute(conn,sql_query)
    
    dbComment(conn,name = c(schema, iloc_nm),type = "table",
              comment = paste0("Infolocs (additional information on locations/steps) for the pgtraj ",
                               dbQuoteIdentifier(conn,pgtraj),"."), display = FALSE)
  
    # add primary key to step_id
    dbAddKey(conn, name = c(schema, iloc_nm), type = "primary", 
        "step_id", display = FALSE)
    # add foreign key to step_id
    sql_query <- paste0("ALTER TABLE ", schemaq, ".", iloc_nmq, 
        " ADD FOREIGN KEY (step_id) REFERENCES ", schemaq, ".step (id)
                    ON UPDATE NO ACTION ON DELETE CASCADE;")
    dbExecute(conn, sql_query)
    message(paste0("Infolocs for pgtraj '", pgtraj, "' written to table '", 
        iloc_nm, "'."))
    return(TRUE)
}

# getPgtrajWithInfo

#' Get pgtraj with infolocs as a data frame (used in pgtraj2ltraj)
#' 
#' @param conn A PostgreSQL connection object.
#' @param pgtraj String, name of the pgtraj
#' @param schema String, name of the schema holding the pgtraj
#' @author David Bucklin \email{dbucklin@@ufl.edu}
#' @keywords internal

getPgtrajWithInfo <- function(conn, pgtraj, schema) {
    
    # DB safe names
    schemaq <- dbQuoteIdentifier(conn, schema)
    
    iloc_nm <- paste0("infolocs_", pgtraj)
    iloc_nmq <- dbQuoteIdentifier(conn, iloc_nm)
    view <- paste0("parameters_",pgtraj)
    viewq <- dbQuoteIdentifier(conn, view)
    
    # get list of bursts (order by id from animal_burst)
    sql_query <- paste0("SELECT burst_name as burst FROM
                          ", schemaq, ".animal_burst a, ", schemaq, ".pgtraj b
                          WHERE a.pgtraj_id = b.id
                          AND b.pgtraj_name = ",dbQuoteString(conn,pgtraj)," 
                          ORDER BY a.id;")
    bursts <- dbGetQuery(conn, sql_query)$burst
    
    # get db tz
    pgtz<-dbGetQuery(conn, "SHOW timezone;")[1,1]
    
    getinfo<-list()
    for (b in 1:length(bursts)) {
      b_nm<-bursts[b]
      # check if defs exist
      sql_query <- paste0("SELECT 1 as test FROM ",schemaq,".animal_burst a, ",schemaq,".pgtraj b 
  			                  WHERE a.pgtraj_id = b.id
                          AND pgtraj_name = ",dbQuoteString(conn,pgtraj), 
                          " AND burst_name = ",dbQuoteString(conn,b_nm), 
                          " AND a.info_cols IS NOT NULL;")
      check <- dbGetQuery(conn, sql_query)$test
    
      if (!is.null(check)) {
          sql_query <- paste0("SELECT unnest(info_cols[1:1]) as nms, 
                            unnest(info_cols[2:2]) as defs,
                            unnest(info_cols[3:3]) as atts FROM "
                            ,schemaq,".animal_burst a, ",schemaq,".pgtraj b WHERE a.pgtraj_id = b.id
                            AND pgtraj_name = ",dbQuoteString(conn,pgtraj), 
                            " AND burst_name = ",dbQuoteString(conn,b_nm),";")
          defs <- dbGetQuery(conn, sql_query)
          
          sql_query <- paste0("SELECT * FROM ", schemaq, ".", iloc_nmq, " WHERE step_id IN
                              	(SELECT s.step_id FROM 
                                  ", schemaq, ".s_b_rel s, ", schemaq, ".animal_burst, ", schemaq, ".pgtraj
                              	  WHERE pgtraj.id = animal_burst.pgtraj_id AND
                              	  animal_burst.id = s.animal_burst_id AND
                              	  pgtraj.pgtraj_name = ",dbQuoteString(conn,pgtraj), " AND 
                                  animal_burst.burst_name = ",dbQuoteString(conn,b_nm),")
                              ORDER BY step_id;")
          # also modify same query below in else{}
          allinfo <- dbGetQuery(conn, sql_query)
          
          # remove step_id
          justinfo <- allinfo[!names(allinfo) == "step_id"]
          
          # assign types
          for (i in names(justinfo)) {
              att <- defs[defs$nms == i, ]
              if (length(att[, 1]) == 0) {
                  next
              }
              if (!is.na(att$atts)) {
                  # handle factors
                  if (att$defs %in% c("factor", "ordered")) {
                    levs <- unlist(strsplit(att$atts, "/*/", fixed = TRUE))
                    ordered <- ifelse(att$defs == "ordered", TRUE, 
                      FALSE)
                    justinfo[, i] <- factor(as.character(justinfo[, 
                      i]), levels = levs[levs != ""], ordered = ordered)
                  }
                  if (att$defs %in% c("POSIXct","POSIXt")) {
                    justinfo[, i] <- list(eval(parse(text = paste0("as.", 
                      att$defs, "(as.character(justinfo[,i]),
                                          tz='", 
                      pgtz, "')"))))
                    # assign R tz
                    eval(parse(
                        text = paste0("attributes(justinfo$",i,")$tzone <- att$atts")
                    ))
                  }
                  if (att$defs == "POSIXlt") {
                    justinfo[, i] <- list(eval(parse(text = paste0("as.", 
                      att$defs, "(as.character(justinfo[,i]),
                                          tz=att$atts)"))))
                  }
              } else {
                  justinfo[, i] <- do.call(paste0("as.", att$defs), 
                    list(justinfo[, i]))
              }
          }
          getinfo[[b]]<-justinfo
      } else {
          sql_query <- paste0("SELECT * FROM ", schemaq, ".", iloc_nmq, " WHERE step_id IN
                              	(SELECT s.step_id FROM 
                                  ", schemaq, ".s_b_rel s, ", schemaq, ".animal_burst, ", schemaq, ".pgtraj
                              	  WHERE pgtraj.id = animal_burst.pgtraj_id AND
                              	  animal_burst.id = s.animal_burst_id AND
                              	  pgtraj.pgtraj_name = ",dbQuoteString(conn,pgtraj), " AND 
                                  animal_burst.burst_name = ",dbQuoteString(conn,b_nm),")
                              ORDER BY step_id;")
          allinfo <- dbGetQuery(conn, sql_query)
          
          # remove step_id
          justinfo <- allinfo[!names(allinfo) == "step_id"]
          
          getinfo[[b]]<-justinfo
      }
    }
return(getinfo)
}

# trajSummaryViews

#' Create views that summarize all pgtrajes (and pgtraj bursts) in a schema.
#' @param conn A PostgreSQL connection object
#' @param schema String, name of the schema
#' @author David Bucklin \email{dbucklin@@ufl.edu}
#' @keywords internal

trajSummaryViews<- function(conn, schema) {
  #pgtraj summary
  sql_query<-paste0("CREATE OR REPLACE VIEW all_pgtraj_summary AS 
                       WITH p AS (
                               SELECT p_1.id,
                                  p_1.pgtraj_name,
                                  p_1.proj4string,
                                  p_1.time_zone,
                                  p_1.note,
                                  p_1.insert_timestamp,
                                  a.table_name
                                 FROM pgtraj p_1
                                   LEFT JOIN ( SELECT tables.table_name
                                         FROM information_schema.tables
                                        WHERE tables.table_schema::text = ",dbQuoteString(conn,schema),
                                        " AND tables.table_name::text ~~ 'infolocs_%'::text) a 
                                        ON p_1.pgtraj_name = substring(a.table_name::text, 10)
                              )
                       SELECT p.id AS pgtraj_id,
                          p.pgtraj_name,
                          p.insert_timestamp AS last_update,
                          count(DISTINCT ab.burst_name) AS num_bursts,
                          count(r.id) AS num_relocations,
                          min(r.relocation_time) AS earliest_date,
                          max(r.relocation_time) AS latest_date,
                          p.table_name::text AS infolocs_table
                         FROM p,
                          animal_burst ab,
                          relocation r,
                          s_b_rel sb,
                          step s
                        WHERE p.id = ab.pgtraj_id 
                        AND ab.id = sb.animal_burst_id 
                        AND sb.step_id = s.id 
                        AND s.relocation_id_1 = r.id
                        GROUP BY p.id, p.pgtraj_name, p.insert_timestamp, p.table_name
                        ORDER BY p.id;")
  dbExecute(conn, sql_query)
  
  # burst summary
  sql_query<-"CREATE OR REPLACE VIEW all_burst_summary AS 
                 SELECT p.id AS pgtraj_id,
                    p.pgtraj_name,
                    ab.animal_name,
                    ab.burst_name,
                    count(r.id) AS num_relocations,
                    count(r.id) - count(r.geom) AS num_na,
                    min(r.relocation_time) AS date_begin,
                    max(r.relocation_time) AS date_end
                   FROM pgtraj p,
                    animal_burst ab,
                    relocation r,
                    s_b_rel sb,
                    step s
                  WHERE p.id = ab.pgtraj_id AND 
                  ab.id = sb.animal_burst_id AND 
                  sb.step_id = s.id AND 
                  s.relocation_id_1 = r.id
                  GROUP BY p.id, p.pgtraj_name, ab.id, ab.animal_name, ab.burst_name
                  ORDER BY p.id, ab.id;"
  dbExecute(conn, sql_query)
  return(invisible())
}


# from rpostgis ------------------------

## dbTableNameFix

##' Format input for database schema/table names.
##'
##' Internal rpostgis function to return common (length = 2) schema
##' and table name vector from various table and schema + table name
##' inputs.
##' 
##' @param conn A connection object. Must be provided but can be set NULL,
##' where a dummy connection will be used.
##' @param t.nm Table name string, length 1-2.
##' @param as.identifier Boolean whether to return (schema,table) name as database
##' sanitized identifiers (TRUE) or as regular character (FALSE)
##' @return character vector of length 2. Each character element is in
##'     (escaped) double-quotes when as.identifier = TRUE.
##' @keywords internal
##' @author David Bucklin
##' @importFrom DBI dbQuoteIdentifier
##' @importFrom DBI dbQuoteString
##' @examples
##' \dontrun{
##' name<-c("schema","table")
##' dbTableNameFix(conn,name)
##' 
##' #current search path schema is added to single-length character object (if only table is given)
##' name<-"table"
##' dbTableNameFix(conn,name)
##' 
##' #schema or table names with double quotes should be given exactly as they are 
##' (make sure to wrap in single quotes in R):
##' name<-c('sch"ema','"table"')
##' dbTableNameFix(conn,name)
##' }

dbTableNameFix <- function(conn=NULL, t.nm, as.identifier = TRUE) {
    ## case of no schema provided
      if (length(t.nm) == 1 && !is.null(conn) && !inherits(conn, what = "AnsiConnection")) {
        schemalist<-dbGetQuery(conn,"select nspname as s from pg_catalog.pg_namespace;")$s
        user<-dbGetQuery(conn,"SELECT current_user as user;")$user
        schema<-dbGetQuery(conn,"SHOW search_path;")$search_path
        schema<-gsub(" ","",unlist(strsplit(schema,",",fixed=TRUE)),fixed=TRUE)
        # use user schema if available
        if ("\"$user\"" == schema[1] && user %in% schemalist) {
          sch<-user
        } else {
          sch<-schema[!schema=="\"$user\""][1]
        } 
        t.nm <- c(sch, t.nm)
      }
      if (length(t.nm) > 2)
      {
        stop("Invalid PostgreSQL table/view name. Must be provided as one ('table') or two-length c('schema','table') character vector.")
      }
    if (is.null(conn)) {conn<-DBI::ANSI()}
    if (!as.identifier) {return(t.nm)} else {
    t.nm<-DBI::dbQuoteIdentifier(conn, t.nm)
    return(t.nm)
    }
}


## dbBuildTableQuery
##' Builds CREATE TABLE query for a data frame object.
##' 
##' @param conn A PostgreSQL connection
##' @param name Table name string, length 1-2.
##' @param obj A data frame object.
##' @param field.types optional named list of the types for each field in \code{obj}
##' @param row.names logical, should row.name of \code{obj} be exported as a row_names field? Default is FALSE
##' 
##' @note Adapted from RPostgreSQL::postgresqlBuildTableDefinition
##' @keywords internal
##' @author David Bucklin

dbBuildTableQuery <- function (conn = NULL, name, obj, field.types = NULL, row.names = FALSE) {
    if (is.null(conn)) {
      conn <- DBI::ANSI()
      nameque <- dbQuoteIdentifier(conn,name)
    } else {
      nameque<-paste(dbTableNameFix(conn, name),collapse = ".")
    }
  
    if (!is.data.frame(obj)) 
        obj <- as.data.frame(obj)
    if (!is.null(row.names) && row.names) {
        obj <- cbind(row.names(obj), obj)
        names(obj)[1] <- "row_names"
    }
    if (is.null(field.types)) {
        field.types <- sapply(obj, dbDataType, dbObj = conn)
    }
    i <- match("row_names", names(field.types), nomatch = 0)
    if (i > 0) 
        field.types[i] <- dbDataType(conn, row.names(obj))
    flds <- paste(dbQuoteIdentifier(conn ,names(field.types)), field.types)
    
    paste("CREATE TABLE ", nameque , "\n(", paste(flds, 
        collapse = ",\n\t"), "\n);")
}

## dbVersion

##' Returns major.minor version of PostgreSQL (for version checking)
##'
##' @param conn A PostgreSQL connection
##' @return numeric vector of length 3 of major,minor,bug version.
##' @keywords internal
##' @author David Bucklin

dbVersion<- function (conn) {
    pv<-dbGetQuery(conn,"SHOW server_version;")$server_version
    nv<-unlist(strsplit(pv,".",fixed=TRUE))
    return(as.numeric(nv))
}


## dbExistsTable
##' Check if a PostgreSQL table/view exists
##' 
##' @param conn A PostgreSQL connection
##' @param name Table/view name string, length 1-2.
##' 
##' @keywords internal
##' @author David Bucklin

dbExistsTable <- function (conn, name, table.only = FALSE) {
    if (!table.only) to<-NULL else to<-" AND table_type = 'BASE TABLE'"
    full.name<-dbTableNameFix(conn,name, as.identifier = FALSE)
    chk<-dbGetQuery(conn, paste0("SELECT 1 FROM information_schema.tables 
               WHERE table_schema = ",dbQuoteString(conn,full.name[1]),
               " AND table_name = ",dbQuoteString(conn,full.name[2]),to,";"))[1,1]
    if (is.null(chk)) {
      exists.t <- FALSE
      # check version (matviews >= 9.3)
      ver<-dbVersion(conn)
      if (!table.only & !(ver[1] < 9 | (ver[1] == 9 && ver[2] < 3))) {
        # matview case - not in information_schema
        chk2<-dbGetQuery(conn, paste0("SELECT oid::regclass::text, relname
                FROM pg_class
                WHERE relkind = 'm'
                AND relname = ",dbQuoteString(conn,full.name[2]),";"))
        if (length(names(chk2)) > 0) {
          sch<-gsub(paste0(".",chk2[1,2]),"",chk2[,1])
          if (full.name[1] %in% sch) exists.t<-TRUE else exists.t<-FALSE
        } else {
          exists.t<-FALSE
        }
      }
    } else {
    exists.t<-TRUE
    }
  return(exists.t)
}


## dbConnCheck
##' Check if a supported PostgreSQL connection
##' 
##' @param conn A PostgreSQL connection
##' 
##' @keywords internal
##' @author David Bucklin

dbConnCheck <- function(conn) {
  if (inherits(conn, c("PostgreSQLConnection"))) {
          return(TRUE)
      } else {
        return(stop("'conn' must be a <PostgreSQLConnection> object."))
      }
}

