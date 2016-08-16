#' Delete one or more pgtrajs from a traj schema.
#' 
#' @description 
#' \code{pgTrajDelete} deletes one or more pgtrajs from a traj schema.
#' 
#' @author Balázs Dukai \email{balazs.dukai@@gmail.com}
#' 
#' @param conn Connection object created with RPostgreSQL
#' @param schema String. Name of the schema that stores the traj data model.
#' @param pgtraj String. A vector containing the names of the pgtrajs.
#' 
#' @return nothing
#' 
#' @examples 
#' \dontrun{pgTrajDelete(conn, "traj", "ibex")}
#' 
#' \dontrun{pgTrajDelete(conn, schema="traj", pgtraj=c("ibex", "puechcirc")}
#' 
#' @export 
#' 

pgTrajDelete <- function(conn, schema = "traj", pgtraj) {
    query <- paste0("")
}