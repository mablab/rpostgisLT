#' Export a pgtraj into an ltraj.
#' 
#' @param conn Connection object created with RPostgreSQL
#' @param schema String. Name of the schema that stores or will store the pgtraj data model.
#' @param pgtraj String. Name of the pgtraj.
#' 
#' @export 
#' 
#' @example
#' pgtraj2ltraj(conn, pgtraj = "ibex") # looks into 'traj' schema by default
#' 
################################################################################
pgtraj2ltraj <- function(conn, schema = "traj", pgtraj) {
    # Begin transaction block
    invisible(dbGetQuery(conn, "BEGIN TRANSACTION;"))
    query <- paste0("SET search_path TO ", schema, ",public;")
    invisible(dbGetQuery(conn, query))
    
    # Refresh view
    query <- paste0("REFRESH MATERIALIZED VIEW ", pgtraj, "_params;")
    invisible(dbGetQuery(conn, query))
    
    # Get parameters
    query <- paste0("SELECT * FROM ", pgtraj, "_params;")
    DF <- invisible(dbGetQuery(conn, query))
    DF$dt <- toSeconds(DF[["dt"]])
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
            burst = DF[["burst"]],
            rowname = DF[["r_rowname"]])
    
    # Cast into ltraj
    ltraj <- dl_opt(DF2)
    
    # Commit transaction and reset search path in the database
    query <- "SET search_path TO \"$user\",public;"
    invisible(dbGetQuery(conn, query))
    dbCommit(conn)
    message(paste0("Ltraj successfully created from ", pgtraj, "."))
    
    return(ltraj)
}

