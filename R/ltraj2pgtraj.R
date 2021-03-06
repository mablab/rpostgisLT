# ltraj2pgtraj

#' Export ltraj object from R to a PostGIS database.
#'
#' \code{ltraj2pgtraj} exports an ltraj to the a database pgtraj,
#' creating a new pgtraj schema if it doesn't exist. The time zone and
#' projection information stored in the ltraj is transferred to the
#' database.
#'
#' @param conn A connection object.
#' @param ltraj An object of class \code{ltraj}.
#' @param schema Character. Name of the schema that stores or will
#'     store the pgtraj data model.
#' @param pgtraj Character. Name of the new \code{pgtraj}. Defaults
#'     to the name of the provided \code{ltraj}.
#' @param note Character. A note that will be stored with the
#'     \code{pgtraj} in the database.
#' @param overwrite Logical. Use if a \code{pgtraj} with the same
#'     name as the provided \code{ltraj} already exists in the
#'     database: If \code{TRUE}, the existing \code{pgtraj} is
#'     deleted and the provided \code{ltraj} is inserted. If
#'     \code{FALSE}, the function exits. Note that \code{overwrite}
#'     requires an exact match among the \code{pgtraj} names
#'     otherwise it is ignored.
#' @param infolocs Logical. Whether to write infolocs to database.
#' @return \code{TRUE} on success.
#' @seealso \code{\link{asPgtraj}} to create a \code{pgtraj} with
#'     data already stored in the database.
#' @aliases writeTraj
#' @author Balázs Dukai \email{balazs.dukai@@gmail.com}
#' @export
#' @examples
#' \dontrun{
#'   # create pgtraj from ltraj "ibex" in schema "traj_t2"
#'   ltraj2pgtraj(conn, ibex, "traj_t2")
#' }

ltraj2pgtraj <-
    function(conn,
             ltraj,
             schema = "traj",
             pgtraj = NULL,
             note = NULL,
             overwrite = FALSE,
             infolocs = TRUE) {
        ## check PostgreSQL connection and PostGIS
        dbConnCheck(conn)
        if (!suppressMessages(pgPostGIS(conn))) {
            stop("PostGIS is not enabled on this database.")
        }
        ## 'pgtraj' defaults to the name of ltraj
        if (is_blank(pgtraj)) {
            pgtraj <- deparse(substitute(ltraj))
        }
        ## only allow pgtraj names that begin with letters or numbers
        if (!grepl("^[0-9A-Za-z]", pgtraj)) {
            stop("Invalid pgtraj name. Valid pgtraj names must begin with a letter or number.")
        }
        ## Check/create pgtraj schema ('pgtrajSchema' has its own
        ## transaction control)
        x <- pgtrajSchema(conn, schema)
        ## If schema creation unsuccessful
        if (!isTRUE(x)) {
            stop("Traj schema couldn't be created, returning from function.")
        }
        ## Checks if 'pgtraj' already exists
        sql_query <-
            paste0("SELECT pgtraj_name FROM ",
                   dbQuoteIdentifier(conn,
                                     schema),
                   ".pgtraj;")
        pgt <- dbGetQuery(conn, sql_query)
        if (pgtraj %in% pgt$pgtraj_name) {
            ## If 'overwrite', drop 'pgtraj', else stop
            if (overwrite) {
                pgtrajDrop(conn, pgtraj, schema, full_clean = FALSE)
            } else {
                stop(
                    paste0(
                        "The pgtraj '",
                        pgtraj,
                        "' already exists in the schema '",
                        schema,
                        "'"
                    )
                )
            }
        }
        ## Otherwise the column 'note' won't be appended to 'dframe'
        ## below
        if (is_blank(note)) {
            note <- NA
        }
        ## Set projection
        srs <- attr(ltraj, "proj4string")
        if (is.null(srs)) {
            srid <- 0
            srs <-
                NA  # not sure this is necessary with updated adehabitatLT (0.3.21)
        } else {
            srid <- suppressMessages(pgSRID(
                conn = conn,
                crs = srs,
                create.srid = TRUE,
                new.srid = NULL
            ))
            srs <- srs@projargs
        }
        ## Get time zone
        time_zone <- attr(ltraj[[1]]$date, "tzone")
        if (is_blank(time_zone)) {
            time_zone <- NA
        }
        ## Convert ltraj to data frame
        dframe <- ld_opt(ltraj)
        ## Get time zone, srs, proj4string, note, pgtraj
        dframe$.time_zone <- time_zone
        dframe$.srid <- srid
        dframe$.proj4string <- srs
        dframe$.pgtraj <- pgtraj
        dframe$.note <- note
        dframe$.burst_order <-
            as.integer(ordered(dframe$burst, burst(ltraj)))
        ## Format date to include time zone that Postgres recognizes, or exclude
        ## 'date' column if the ltraj is of Type I
        type <- attr(ltraj, "typeII") # TRUE if Type II
        if (type) {
            dframe$date <- sapply(dframe$date, function(x)
                strftime(
                    x,
                    format = "%Y-%m-%d %H:%M:%S",
                    tz = "",
                    usetz = TRUE
                ))
        } else {
            dframe$date <- NA
            dframe$dt <- NA
            # TODO: workaround in ltraj_insert.sql
            # It would be more elegant to exclude these columns from the temporary
            # table since the beginning, but the current import procedure in
            # 'insert_ltraj.sql' does not support that. One potential solution
            # that I found is to use the CASE conditional to check if the 'date'
            # column exists in the temporary table and if so insert its value,
            # else insert NULL. However, CASE cannot check for non-existing
            # columns...So a workaround could be build along these answers:
            # http://dba.stackexchange.com/questions/66741/why-cant-i-use-a-case-statement-to-see-if-a-column-exists-and-not-select-from-i
            # http://stackoverflow.com/questions/11472790/postgres-analogue-to-cross-apply-in-sql-server#35873193
        }
        ## Parameters to exclude on input
        params <- c("dist", "abs.angle")
        ## Begin transaction block and input to postgres
        invisible(dbExecute(conn, "BEGIN TRANSACTION;"))
        ## Set database search path
        current_search_path <- dbGetQuery(conn, "SHOW search_path;")
        sql_query <-
            paste0("SET search_path TO ",
                   dbQuoteIdentifier(conn,
                                     schema),
                   ",public;")
        invisible(dbExecute(conn, sql_query))
        ## Import data frame into a temporary table
        res <- tryCatch({
            invisible(dbWriteTable(
                conn,
                name = "zgaqtsn_temp",
                value = dframe[,-which(names(dframe) %in% params)],
                row.names = FALSE
            ))
            TRUE
        }, warning = function(x) {
            message(x)
            message(". Rolling back transaction")
            dbRollback(conn)
            stop("Returning from function")
        }, error = function(x) {
            message(x)
            message(". Rolling back transaction")
            dbRollback(conn)
            stop("Returning from function")
        })
        ## Run the SQL import script to insert the data from the
        ## temporary table into the traj schema
        res2 <- tryCatch({
            pgtraj_insert_file <- system.file("sql/insert_ltraj.sql", package = "rpostgisLT")
            pif <- readLines(pgtraj_insert_file, encoding = "UTF-8")
            pif[pif == ""] <- "%SPLITHERE%"
            pif <- paste(pif, collapse = "\n")
            sql_query <- unlist(strsplit(pif, "%SPLITHERE%", fixed = TRUE))
            for (sq in sql_query) {
                invisible(dbExecute(conn, sq))
            }
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
        res <- c(res, res2)
        ## Drop temporary table
        invisible(dbExecute(conn, "DROP TABLE zgaqtsn_temp;"))
        ## Create parameter and geometry views
        res3 <- tryCatch({
            pgTrajViewParams(conn, schema, pgtraj, srid, db = FALSE)
            ## TODO create view if doesn't exist
            pgTrajViewStepGeom(conn, schema, pgtraj)
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
        ## Commit transaction and restore search path
        tryCatch({
            if (all(res)) {
                dbCommit(conn)
                message(
                    paste0(
                        "The ltraj '",
                        pgtraj,
                        "' has been successfully inserted into the database schema '",
                        schema,
                        "'."
                    )
                )
                ## Vacuum the tables
                suppressMessages(pgtrajVacuum(conn, schema))
                ## infolocs writing
                if (infolocs) {
                    info <- NULL
                    try(info <- writeInfoFromLtraj(conn, ltraj, pgtraj,
                                                   schema))
                    if (is.null(info))
                        message("Infolocs writing for pgtraj '",
                                pgtraj,
                                "' failed.")
                }
                ## Restore database search path
                sql_query <-
                    paste0("SET search_path TO ", current_search_path,
                           ";")
                invisible(dbExecute(conn, sql_query))
                return(TRUE)
            } else {
                dbRollback(conn)
                stop("Ltraj insert failed.")
            }
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
    }