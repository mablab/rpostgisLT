#' Import a pgtraj into an ltraj.
#' 
#' @description 
#' \code{pgtraj2ltraj} imports a single pgtraj from a database into an ltraj object.
#' 
#' @author Balázs Dukai \email{balazs.dukai@@gmail.com}
#' 
#' @param conn Connection object created with RPostgreSQL
#' @param schema String. Name of the schema that stores or will store the pgtraj data model.
#' @param pgtraj String. Name of the pgtraj.
#' 
#' @return an ltraj object
#' 
#' @examples 
#' \dontrun{pgtraj2ltraj(conn, "traj_t2", "ibex")}
#' 
#' @export 
#' 
################################################################################
pgtraj2ltraj <- function(conn, schema = "traj", pgtraj) {
    # Begin transaction block
    invisible(RPostgreSQL::dbGetQuery(conn, "BEGIN TRANSACTION;"))
    current_search_path <- RPostgreSQL::dbGetQuery(conn, "SHOW search_path;")
    query <- paste0("SET search_path TO ", schema, ",public;")
    invisible(RPostgreSQL::dbGetQuery(conn, query))
    
    # Get parameters
    query <- paste0("SELECT * FROM ", pgtraj, "_params;")
    DF <- invisible(RPostgreSQL::dbGetQuery(conn, query))
    query <- paste0("SELECT r_tz FROM pgtrajs WHERE p_name = '",pgtraj,"';")
    tz <- dbGetQuery(conn, query)[1,1]
    
    DF2 <- data.frame(
            x = DF[["x"]],
            y = DF[["y"]],
            date = DF[["date"]],
            dx = DF[["dx"]],
            dy = DF[["dy"]],
            dist = DF[["dist"]],
            dt = DF[["dt"]],
            R2n = DF[["r2n"]],
            abs.angle = DF[["abs_angle"]],
            rel.angle = DF[["rel_angle"]],
            id = DF[["id"]],
            burst = DF[["burst"]])
    
    # Set row names
    rownames(DF2) <- DF[["r_rowname"]]
    
    # Set time zone
    attr(DF2$date, "tzone") <- tz
    
    # Cast into ltraj
    ltraj <- dl_opt(DF2)
    
    # Commit transaction and reset search path in the database
    query <- paste0("SET search_path TO ", current_search_path, ";")
    invisible(RPostgreSQL::dbGetQuery(conn, query))
    RPostgreSQL::dbCommit(conn)
    message(paste0("Ltraj successfully created from ", pgtraj, "."))
    
    return(ltraj)
}

