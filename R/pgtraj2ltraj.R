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
#' @importFrom stats complete.cases
#' @importFrom sp CRS
#' 
#' @examples 
#' \dontrun{pgtraj2ltraj(conn, "traj_t2", "ibex")}
#' 
#' @export 
#' 
##############################################################################
pgtraj2ltraj <- function(conn, schema = "traj", pgtraj) {
    
    # sanitize schema name
    schema_q <- dbQuoteIdentifier(conn,schema)
  
    view <- paste0(pgtraj, "_parameters")
    DF <- invisible(dbReadTable(conn, c(schema, view)))
    
    # Get time zone
    sql_query <- paste0("SELECT time_zone FROM ",schema_q,".pgtraj WHERE pgtraj_name = ",dbQuoteString(conn, pgtraj),";")
    tz <- dbGetQuery(conn, sql_query)[1,1]
    
    # Get proj4string
    sql_query <- paste0("SELECT proj4string FROM ",schema_q,".pgtraj WHERE pgtraj_name = ",dbQuoteString(conn, pgtraj),";")
    proj4string <- dbGetQuery(conn, sql_query)[1,1]
    
    # Rename and prepare data frame for conversion to ltraj
    names(DF)[names(DF)=="r2n"] <- "R2n"
    names(DF)[names(DF)=="abs_angle"] <- "abs.angle"
    names(DF)[names(DF)=="rel_angle"] <- "rel.angle"
    names(DF)[names(DF)=="animal_name"] <- "id"
    
    DF <- DF[,-which(names(DF)=="pgtraj")]
    
    # Check if the row names are stored in the pgtraj
    rnames <- all(stats::complete.cases(DF$r_rowname))
    if (rnames) {
        names(DF)[names(DF)=="r_rowname"] <- "r.row.names"
    } else {
        DF <- DF[,-which(names(DF)=="r_rowname")]
    }
    
    # Set time zone
    attr(DF$date, "tzone") <- tz
    
    # Cast into ltraj
    ltraj <- dl_opt(DF, rnames)
    
    if (proj4string %in% c("NA", "NULL", "NaN")) {
        attr(ltraj, "proj4string") <- sp::CRS()
    } else {
        attr(ltraj, "proj4string") <- sp::CRS(proj4string)
    }
    
    message(paste0("Ltraj successfully created from ", pgtraj, "."))
    
    return(ltraj)
}

