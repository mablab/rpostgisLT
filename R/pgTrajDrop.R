# pgtrajDrop

#' Delete a pgtraj/unused rows from a traj schema.
#' 
#' \code{pgtrajDrop} deletes a pgtraj and/or all unused rows from a traj schema.
#' 
#' @param conn Connection object created with RPostgreSQL
#' @param pgtraj String. Name of the pgtraj (can be left NULL to perform full_clean)
#' @param schema String. Name of the schema storing the pgtraj
#' @param full_clean String. Whether to delete all unused rows in 'relocation' table.
#'    Should be done regularly if frequently overwriting many pgtraj, 
#'    but note that it can take a long time to run.
#' 
#' @return TRUE on success
#' 
#' @author Balázs Dukai \email{balazs.dukai@@gmail.com}
#' @export
#' 
#' @examples 
#' \dontrun{
#'   # drop "ibex" pgtraj in schema "traj"
#'   pgtrajDrop(conn, "ibex")
#'   
#'   # clean "traj" schema by deleting all unused rows in "relocation" table
#'   pgtrajDrop(conn)
#' }

pgtrajDrop <- function(conn, pgtraj = NULL, schema = "traj", full_clean = TRUE) {
    
    ## check PostgreSQL connection
    rpostgis:::dbConnCheck(conn)
    ## check that action is requested
    if (is.null(pgtraj) && !full_clean) stop("No action specified: either pgtraj must be specified, or full_clean must be TRUE.")
    ## Set database search path
    current_search_path <- dbGetQuery(conn, "SHOW search_path;")
    sql_query <- paste0("SET search_path TO ", dbQuoteIdentifier(conn, 
        schema), ",public;")
    invisible(dbExecute(conn, sql_query))
    
    # Vacuum the schema for full_cleans
    if (full_clean) {
      suppressMessages(pgtrajVacuum(conn, schema, full = FALSE, 
      verbose = FALSE, analyze = TRUE))
    }
    
    if (!is.null(pgtraj)) {
      sql_query <- paste0("SELECT id FROM pgtraj WHERE pgtraj_name = ", 
          dbQuoteString(conn, pgtraj), ";")
      i <- dbGetQuery(conn, sql_query)[1, 1]
      if (is.null(i)) {
          stop(paste("The pgtraj '", pgtraj, "' doesn't exist in schema '", 
              schema, "'."))
      }
      
      # Drop query (multiple queries in statment)
      sql_query <- c(paste0("DELETE FROM pgtraj WHERE pgtraj_name = ", 
          dbQuoteString(conn, pgtraj), ";"),
          paste0("DROP VIEW ", dbQuoteIdentifier(conn, paste0("parameters_", pgtraj)), ";"),
          paste0("DROP VIEW ", dbQuoteIdentifier(conn, paste0("step_geometry_", pgtraj)), ";"))
    }
    
    # Begin transaction block
    invisible(dbExecute(conn, "BEGIN TRANSACTION;"))
    
    tryCatch({
      # delete pgtraj
      if (!is.null(pgtraj)) {
        for (sq in sql_query){
          invisible(dbExecute(conn, sq))
        }
        if (rpostgis:::dbExistsTable(conn, paste0("infolocs_", 
            pgtraj))) {
            dbDrop(conn, c(schema, paste0("infolocs_", pgtraj)), 
                type = "table", display = FALSE)
        }
      }
      # delete all unused rows in relocation
        if (full_clean) {
            message("Deleting all unused rows from pgtraj schema tables, please be patient...")
            sql_query <- "DELETE FROM relocation WHERE id NOT IN (SELECT
                  r1.id AS relocation_id
                  FROM step s
                  JOIN relocation r1 ON s.relocation_id_1 = r1.id
                  LEFT JOIN relocation r2 ON s.relocation_id_2 = r2.id
                  JOIN s_b_rel rel ON rel.step_id = s.id
                  JOIN animal_burst ab ON ab.id = rel.animal_burst_id
                  JOIN pgtraj p ON p.id = ab.pgtraj_id
                  WHERE p.pgtraj_name IN (SELECT pgtraj_name from pgtraj)
                  AND r1.id IS NOT NULL
                  UNION
                  SELECT
                    r2.id AS relocation_id
                  FROM step s
                  JOIN relocation r1 ON s.relocation_id_1 = r1.id
                  LEFT JOIN relocation r2 ON s.relocation_id_2 = r2.id
                  JOIN s_b_rel rel ON rel.step_id = s.id
                  JOIN animal_burst ab ON ab.id = rel.animal_burst_id
                  JOIN pgtraj p ON p.id = ab.pgtraj_id
                  WHERE p.pgtraj_name IN (SELECT pgtraj_name from pgtraj)
                  AND r2.id IS NOT NULL);"
            rws<-dbExecute(conn, sql_query)
        }
        dbCommit(conn)
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
    
    # Restore database search path
    sql_query <- paste0("SET search_path TO ", current_search_path, 
        ";")
    invisible(dbExecute(conn, sql_query))
    
    # Vacuum the schema
    suppressMessages(pgtrajVacuum(conn, schema, full = FALSE, 
        verbose = FALSE, analyze = TRUE))
    
    if (!is.null(pgtraj)) {
        message(paste0("The pgtraj '", pgtraj, "' has been deleted from the database schema '", 
        schema, "'.")) } else {
        message(paste0(rws, " rows were deleted."))
        }
    return(TRUE)
}