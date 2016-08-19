#' Delete one or more pgtrajs from a traj schema.
#' 
#' @description 
#' \code{pgTrajDrop} deletes one or more pgtrajs from a traj schema.
#' 
#' @author Balázs Dukai \email{balazs.dukai@@gmail.com}
#' 
#' @param conn Connection object created with RPostgreSQL
#' @param schema String. Name of the schema that stores the traj data model.
#' @param pgtraj String. A vector containing the names of the pgtrajs.
#' 
#' @return TRUE on success
#' 
#' @examples 
#' \dontrun{pgTrajDrop(conn, "traj", "ibex")}
#' 
#' \dontrun{pgTrajDrop(conn, schema="traj", pgtraj=c("ibex", "puechcirc")}
#' 
#' @export 
#' 

pgTrajDrop <- function(conn, schema = "traj", pgtraj) {
    
    # Set database search path
    current_search_path <- dbGetQuery(conn, "SHOW search_path;")
    query <- paste0("SET search_path TO ", schema, ",public;")
    invisible(dbGetQuery(conn, query))
    
    query <- paste0("
                    DELETE FROM infoloc WHERE id IN (
                        SELECT
                            i.id AS infoloc_id
                        FROM infoloc i
                        JOIN s_i_b_rel rel ON rel.infoloc_id = i.id
                        JOIN animal_burst ab ON ab.id = rel.animal_burst_id
                        JOIN pgtraj p ON p.id = ab.pgtraj_id
                        WHERE p.pgtraj_name = '",pgtraj,"'
                        );
                    
                    DELETE FROM relocation WHERE id IN (
                        SELECT
                            r1.id AS relocation_id
                        FROM step s
                        JOIN relocation r1 ON s.relocation_id_1 = r1.id
                        LEFT JOIN relocation r2 ON s.relocation_id_2 = r2.id
                        JOIN s_i_b_rel rel ON rel.step_id = s.id
                        JOIN animal_burst ab ON ab.id = rel.animal_burst_id
                        JOIN pgtraj p ON p.id = ab.pgtraj_id
                        WHERE p.pgtraj_name = '",pgtraj,"'
                        );
                        
                    DELETE FROM pgtraj WHERE pgtraj_name = '",pgtraj,"';
                    ")
    query <- gsub(pattern = '\\s', replacement = " ", x = query)
    
    # Begin transaction block
    invisible(dbSendQuery(conn, "BEGIN TRANSACTION;"))
    
    tryCatch({
            
            invisible(dbSendQuery(conn, query))
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
    query <- paste0("SET search_path TO ", current_search_path, ";")
    invisible(dbSendQuery(conn, query))
    
    message(paste0("The pgtraj '", pgtraj, "' deleted from the database schema '", schema,"'."))
    return(TRUE)

}
