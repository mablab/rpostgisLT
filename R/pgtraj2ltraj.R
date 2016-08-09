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
    
    # Get parameters
#    query <- paste0("SELECT * FROM ",schema,".", pgtraj, "_params;")
#    DF <- invisible(dbGetQuery(conn, query))
    
    view <- paste0(pgtraj, "_params")
    DF <- invisible(dbReadTable(conn, c(schema, view)))
    
    query <- paste0("SELECT time_zone FROM ",schema,".pgtraj WHERE pgtraj_name = '",pgtraj,"';")
    tz <- dbGetQuery(conn, query)[1,1]
    
    query <- paste0("SELECT proj4string FROM ",schema,".pgtraj WHERE pgtraj_name = '",pgtraj,"';")
    proj4string <- dbGetQuery(conn, query)[1,1]
    
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
            id = DF[["animal_name"]],
            burst = DF[["burst"]],
            r.row.names = DF[["r_rowname"]])
    
    # Set time zone
    attr(DF2$date, "tzone") <- tz
    
    # Cast into ltraj
    ltraj <- dl_opt(DF2)
    
    if (proj4string %in% c("NA", "NULL", "NaN")) {
        attr(ltraj, "proj4string") <- CRS()
    } else {
        attr(ltraj, "proj4string") <- CRS(proj4string)
    }
    
    message(paste0("Ltraj successfully created from ", pgtraj, "."))
    
    return(ltraj)
}

