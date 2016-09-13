#' Import a pgtraj into an ltraj.
#' 
#' @description 
#' \code{pgtraj2ltraj} imports a single pgtraj from a database into an ltraj object.
#' 
#' @author Balázs Dukai \email{balazs.dukai@@gmail.com}
#' 
#' @param conn Connection object created with RPostgreSQL
#' @param pgtraj String. Name of the pgtraj.
#' @param schema String. Name of the schema that stores or will store the pgtraj data model.
#' 
#' @return an ltraj object
#' 
#' @importFrom stats complete.cases
#' @importFrom sp CRS
#' 
#' @examples 
#' \dontrun{
#'  # create ltraj from pgtraj named "ibex" in schema "traj_t2"
#'  ibex<-pgtraj2ltraj(conn, "ibex", "traj_t2")
#' }
#' 
#' @export 
#' 
##############################################################################
pgtraj2ltraj <- function(conn, pgtraj, schema = "traj") {
    
    ## check PostgreSQL connection
    if (!inherits(conn, "PostgreSQLConnection"))
        stop("'conn' should be a PostgreSQL connection.")
    # sanitize schema name
    schema_q <- dbQuoteIdentifier(conn,schema)
  
    view <- paste0(pgtraj, "_parameters")
    view_q <- dbQuoteIdentifier(conn,view)
    
    # check if infolocs exist
   if (dbExistsTable(conn,c(schema,paste0("z_infolocs_",pgtraj)))) {
      DF <- getPgtrajWithInfo(conn, pgtraj, schema)
    } else {
      DF <- invisible(dbReadTable(conn, c(schema, view)))
    }
    #remove step_id column
    DF$step_id<- NULL
    
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

