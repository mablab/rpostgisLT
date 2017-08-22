# as_pgtraj

#' Imports location data from a database table into the pgtraj database model
#' 
#' \code{as_pgtraj} populates a \code{pgtraj} schema from the data provided
#' in \code{relocations_table}. If the provided schema doesn't exist, it will 
#' be created. On successful data input, \code{as_pgtraj} creates two database 
#' views for each new pgtraj. These views are named parameters_<pgtraj_name>, 
#' step_geometry_<pgtraj_name> and described in more detail in the package 
#' vignette.
#' 
#' Opening and closing PostgreSQL connections have to be done manually by the user. 
#' However, the function checks if the provided connection is still valid. 
#' 
#' Note that the arguments \code{pgtrajs}, \code{animals}, \code{bursts}, and 
#' \code{note} can refer to either a column name in \code{relocations_table},
#' or a string value. If the value is a column name, the values for the corresponding
#' attribute (e.g., \code{animals}) will be the values from that column.
#' When providing a string value, the value will be applied to that attribute for 
#' the entire \code{pgtraj}.
#' 
#' Burst names must be unique across a pgtraj. If it is not desired to further 
#' subset individual animal trajectories, leave \code{bursts = NULL},
#' in which case burst names will be equal to the animal name.
#' 
#' The time zone of the pgtraj is set to the local time zone of the user.
#' 
#' @seealso Section on pgtraj data model in the package vignette. 
#' 
#' @references \url{https://CRAN.R-project.org/package=adehabitatLT/vignettes/adehabitatLT.pdf}
#' 
#' @param conn Connection object created with RPostgreSQL
#' @param relocations_table String. Name of the schema and table that stores 
#'    the relocations, e.g. c("schema","relocations")
#' @param schema String. Name of the schema that stores or will store the 
#'    pgtraj data model (Default = "traj").
#' @param pgtrajs String. Name of the pgtraj or name of the field that 
#'    stores the pgtraj names.
#' @param animals String. Name of the animal or name of the field that 
#'    stores the animal names.
#' @param bursts String. (Optional) name of the burst or name of the field
#'    that stores the burst names. If not given, each animal will have one 
#'    burst.
#' @param relocations String. Name of the field that contains the relocations 
#'    in relocations_table. Relocations can be provided either as columns names
#'    containing X,Y coordinates (e.g., \code{c("x","y")}) or a PostGIS geometry
#'    (e.g., \code{"geom"}). In both cases all relocations in relocations_table 
#'    must have the same projection. If provided as coordinates in two columns,
#'    projection will be undefined unless \code{srid} is defined.
#' @param timestamps String. Name of the field in relocations_table that 
#'    contains the timestamps. If NULL, Type I trajectory is assumed.
#' @param rids String. Name of the field in relocations_table that contains 
#'    the numeric IDs of relocations. If \code{timestamps = NULL}, relocations 
#'    will be sorted by the ascending numeric IDs in this field.
#' @param srid Integer. Optional SRID (spatial reference ID) of (x,y)
#'    coordinates provided for relocations. Ignored if relocations is a 
#'    geometry type.
#' @param tzone String. Time zone specification for the timestamps column. If not
#'    specified, the database server time zone will be used (usually the server's local
#'    time zone).
#' @param note String. Comment on the pgtraj. The comment is only used in
#'    the database and not transferred into the ltraj.
#' @param clauses character, additional SQL to append to modify data 
#'    selected from relocations_table. Must begin with \code{WHERE ...}, 
#'    and cannot contain \code{ORDER BY} or \code{LIMIT} clauses.
#' @param info_cols String. Optional character vector of database table 
#'    column names storing additional information on relocations 
#'    (replicating "infolocs" from the \code{adehabitatLT} object \code{ltraj}).
#' @param info_table Character vector of \code{c("schema","table")} holding the 
#'    \code{info_cols}. If \code{info_cols} are in \code{relocations_table}, 
#'    leave NULL.
#' @param info_rids String. Column name of unique integer ID in \code{info_table} 
#'    to join with \code{rids} from \code{relocations_table}. If \code{info_cols} 
#'    are in \code{relocations_table}, leave NULL.
#' @return TRUE on success
#' 
#' @author Balázs Dukai \email{balazs.dukai@@gmail.com}
#' @author David Bucklin \email{dbucklin@@ufl.edu}
#' 
#' @export 
#' 
#' @examples 
#' \dontrun{
#' as_pgtraj(conn, 
#'         relocations_table = c("example_data","relocations_plus"),
#'         pgtrajs = "id",
#'         animals = "animal",
#'         bursts = "burst",
#'         relocations = "geom",
#'         timestamps = "time",
#'         rids = "gid",
#'         note = "trajectories in 2015",
#'         clauses = "WHERE extract(year FROM acquisition_time) = 2015",
#'         info_cols = c("dist_to_road","land_cover","error_class")
#'         )
#' }
#' 
#' \dontrun{
#' as_pgtraj(conn, 
#'         relocations_table = c("example_data","relocations_plus"),
#'         schema = "traj_t4",
#'         pgtrajs = "id",
#'         animals = "animal",
#'         bursts = "burst",
#'         relocations = c("x","y"),
#'         timestamps = "time",
#'         rids = "gid")
#' }

as_pgtraj <- function(conn, relocations_table, schema = "traj", 
    pgtrajs = "pgtraj", animals = "animal", bursts = NULL, relocations, 
    timestamps = NULL, rids = "rid", srid = NULL, tzone = NULL, note = NULL, 
    clauses = NULL, info_cols = NULL, info_table = NULL, info_rids = NULL) {
    ## check PostgreSQL connection and PostGIS
    rpostgis:::dbConnCheck(conn)
    if (!suppressMessages(pgPostGIS(conn))) {
        stop("PostGIS is not enabled on this database.")
    }
    
    if(length(relocations_table) > 0) {
        if (!RPostgreSQL::dbExistsTable(conn_data, relocations_table)) {
            stop(paste("Couldn't find the table",
                       paste(relocations_table, collapse = "."),
                       "in the database."))
        }
    } else {
        stop("Please provide a value for relocations_table.")
    }
    
    # Ensure length-2 table names (search path changes throughout fn)
    relocations_table <- rpostgis:::dbTableNameFix(conn, relocations_table, 
        as.identifier = FALSE)
    
    if (!is.null(info_table)) {
        if (!RPostgreSQL::dbExistsTable(conn_data, info_table)) {
                stop(paste("Couldn't find the table",
                           paste(info_table, collapse = "."),
                           "in the database."))
        }
        
        info_table <- rpostgis:::dbTableNameFix(conn, info_table, 
                                                as.identifier = FALSE)
    }
    
    # sanitize table name
    relocations_table_q <- paste(rpostgis:::dbTableNameFix(conn, 
        relocations_table), collapse = ".")
    # sanitize column name strings used in queries
    relocations_q <- dbQuoteIdentifier(conn, relocations)
    f <- DBI::dbListFields(conn, relocations_table)
    if(!(relocations %in% f)) {
        stop(paste("The field", relocations, "is not present in the table",
                   relocations_table))
    }
    
    # adjust for additional SQL in clauses
    if (!is.null(clauses)) {
        w_a <- " AND "
    } else {
        w_a <- " WHERE "
    }
    
    ##### Test inputs (connection, table, field and values)
    sql_query <- paste0("SELECT ", relocations_q[1], " FROM ", 
        relocations_table_q, " ", clauses, w_a, relocations_q[1], 
        " IS NOT NULL LIMIT 1;")
    a <- suppressWarnings(dbGetQuery(conn, sql_query)[1, 1])
    if (is.null(a)) {
        print(paste("Field", relocations, "does not contain values."))
    }
    
    # Check if the relocation geometry is projected
    if (length(relocations) == 1) {
        sql_query <- paste0("SELECT ST_SRID(", relocations_q, 
            ") FROM ", relocations_table_q, " ", clauses, w_a, 
            relocations_q[1], " IS NOT NULL LIMIT 1;")
        srid <- dbGetQuery(conn, sql_query)[1, 1]
    } else {
        # if relocations are provided as X,Y coordinates
        if (is.null(srid)) {
          srid <- 0
        }
    }
    # ask to continue if srid = 0
    if (srid == 0) {
            acr <- NA
            while (is.na(acr) | !(acr %in% "y" | acr %in% "n")) {
                acr <- readline("The projection of the data is not defined. Do you want to continue? [y/n]")
                acr <- ifelse(grepl("y|n", acr), acr, as.character(acr))
            }
            if (acr %in% "n") {
                stop("Projection is not set, returning from function.")
            }
    } 
    
    # Select proj4text from 'spatial_ref_sys'
    sch <- dbGetQuery(conn, "SELECT schemaname FROM pg_tables WHERE tablename = 'spatial_ref_sys';")[1,1]
    sql_query <- paste0("SELECT proj4text FROM ", sch, ".spatial_ref_sys WHERE srid = ", 
        srid, ";")
    proj4string <- dbGetQuery(conn, sql_query)[1,1]
    
    # Get user local time zone for temporary table
    if (is.null(tzone)) {
      time_zone <- dbGetQuery(conn, "SHOW timezone;")$TimeZone
    } else {
      if (!tzone %in% OlsonNames()) stop(paste0("Invalid time zone name (",
                                                tzone,"). Run `OlsonNames()` for a list of valid names."))
      time_zone <- tzone
    }
    
    # Create traj database schema if it doesn't exist
    x <- pgtrajSchema(conn, schema)
    # If schema creation unsuccessful
    if (!isTRUE(x)) {
        stop("Traj schema couldn't be created, returning from function...")
    }
    
    ##### Insert data into temporary table Begin transaction block
    invisible(dbExecute(conn, "BEGIN TRANSACTION;"))
    
    # Create temporary table 'zgaqtsn_temp'
    res0 <- tryCatch({
      pgTrajTempT(conn, schema)
    }, warning = function(x) {
      message(x)
      message(" . Rolling back transaction")
      dbRollback(conn)
      stop("Returning from function")
    }, error = function(x) {
      message(x)
      message(" . Rolling back transaction")
      dbRollback(conn)
      stop("Returning from function")
    })
    
    # Insert values into 'zgaqtsn_temp'
    res1 <- tryCatch({
      pgTrajDB2TempT(
        conn,
        schema,
        relocations_table,
        pgtrajs,
        animals,
        bursts,
        relocations,
        timestamps,
        rids,
        srid,
        proj4string,
        note,
        clauses,
        time_zone
      )
    }, warning = function(x) {
      message("WARNING in insert into the temporary table:")
      message(x)
      message(" . Rolling back transaction")
      dbRollback(conn)
      stop("Returning from function")
    }, error = function(x) {
      message("ERROR in insert into the temporary table:")
      message(x)
      message(" . Rolling back transaction")
      dbRollback(conn)
      stop("Returning from function")
    })
    
    res <- c(res0, res1)
    
    ##### Insert relocations from the temporary table into the schema
    
    # Set search path in the database
    current_search_path <- dbGetQuery(conn, "SHOW search_path;")
    sql_query <- paste0("SET search_path TO ", dbQuoteIdentifier(conn, 
        schema), ",public;")
    invisible(dbExecute(conn, sql_query))
    
    # Run the SQL import script to insert the data from the
    # temporary table into the traj schema
    res2 <- tryCatch({
      if (is.null(timestamps)) {
        type <- 1
      } else {
        type <- 2
      }
      invisible(dbExecute(conn, paste0("SELECT insert_pgtraj(", type, ");")))
      TRUE
    }, warning = function(x) {
      message(x)
      message(" . Rolling back transaction")
      dbRollback(conn)
      stop("Returning from function")
      
    }, error = function(x) {
      message(x)
      message(". Rolling back transaction")
      dbRollback(conn)
      stop("Returning from function")
    })
    
    # Create views FIXME remove suppressWarnings
    if (suppressWarnings(all(res))) {
      pgt <-dbGetQuery(conn, "SELECT DISTINCT pgtraj_name FROM zgaqtsn_temp;")[,1]
      for (i in pgt) {
        res3 <- tryCatch({
          pgTrajViewParams(conn, schema, pgtraj = i, srid,
                           db = TRUE)
          
          # TODO create view if doesn't exist
          pgTrajViewStepGeom(conn, schema, pgtraj = i)
          
        }, warning = function(x) {
          message(x)
          message(" . Rolling back transaction")
          dbRollback(conn)
          stop("Returning from function")
          
        }, error = function(x) {
          message(x)
          message(" . Rolling back transaction")
          dbRollback(conn)
          stop("Returning from function")
          
        })
        res <- c(res, res3)
      }
    }
    
    # Commit transaction, infolocs if specified
    if (suppressWarnings(all(res))) {
        # infolocs
        if (!is.null(info_cols)) {
            suppressMessages(dbExecute(conn, "ANALYZE zgaqtsn_temp;"))
            pgtraj_list <- dbGetQuery(conn, "SELECT DISTINCT pgtraj_name as p FROM zgaqtsn_temp;")$p
            if (is.null(info_rids)) 
                info_rids <- rids
            if (is.null(info_table)) 
                info_table <- relocations_table
            for (p in pgtraj_list) {
                info <- FALSE
                try(info <- writeInfoFromDB(conn, pgtraj = p, 
                  schema, info_cols, info_table, info_rids))
                if (!info) 
                  message("Infolocs writing for pgtraj '", p, 
                    "' failed.")
            }
        }
        # commit transaction (drops temp table)
        dbCommit(conn)
        # Vacuum the tables
        suppressMessages(pgtrajVacuum(conn, schema))
        # reset search path in the database
        sql_query <- paste0("SET search_path TO ", current_search_path, 
            ";")
        invisible(dbExecute(conn, sql_query))
        # Return TRUE
        return(all(res))
    } else {
        message("Insert failure, rolling back transaction")
        dbRollback(conn)
        # reset search path in the database
        sql_query <- paste0("SET search_path TO ", current_search_path, 
            ";")
        invisible(dbExecute(conn, sql_query))
    }
}