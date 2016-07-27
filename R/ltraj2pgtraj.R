#' Export an ltraj object from R into a traj database schema.
#' 
#' @description 
#' \code{ltraj2pgtraj} creates a new traj schema or uses an existing one and 
#' exports an ltraj to the database. The time zone and projection information
#' stored in the ltraj is transferred to the database. Uses \code{as_pgtraj} 
#' to insert the values into the traj schema.
#' 
#' @author Balázs Dukai \email{balazs.dukai@@gmail.com}
#' 
#' @param ltraj An ltraj object.
#' @param conn Connection object created with RPostgreSQL
#' @param schema String. Name of the schema that stores or will store the pgtraj data model.
#' @param pgtraj String. Name of the new pgtraj. Defaults to the name of the 
#' variable that stores the ltraj.
#' @param comment String. A comment that will be stored with the pgtraj in the database.
#' @param create Logical. If no matching SRID is found, should a new SRID be created? 
#' User must have write access on spatial_ref_sys table.
#' @param new.srid Integer. Optional SRID to give to a newly created SRID. If left NULL (default),
#' the next open value of `srid` in `spatial_ref_sys` between 880000 and 890000 will be used.
#' 
#' @return TRUE on success
#' 
#' @seealso \code{\link{as_pgtraj}}
#' 
#' @examples 
#' \dontrun{ltraj2pgtraj(conn, ibex, "traj_t2")}
#' 
#' @export 
#' 
# TODO once SRID is stored in the ltraj, include that too
################################################################################
ltraj2pgtraj <- function(conn, ltraj, schema = "traj", pgtraj = NULL, 
        comment = NULL, create = FALSE, new.srid = NULL) {
    # 'pgtraj' defaults to the name of ltraj
    if (is.null(pgtraj)) {
        pgtraj <- deparse(substitute(ltraj))
    }
    
    # Set projection
    srs <- attr(ltraj, "proj4string")
    if (is.na(srs)) {
        srid <- 0
    } else {
        srid <- pgSRID(srs, conn, create = create, new.srid = new.srid)
    }
    
    # Get time zone
    tz <- attr(ltraj[[1]]$date, "tzone")
    
    # Convert ltraj to data frame
    dframe <- ld_opt(ltraj)
    
    # Check and create a pgtraj schema
    x <- pgTrajSchema(conn, schema)
    # If schema doesn't exists and user doesn't want to create it
    if (x == "Exit") {
        stop("Returning from function...")
    }
    
    # Import data frame into a temporary table
    pgTrajTempT(conn, schema)
    test <- suppressMessages(pgTrajR2TempT(conn, schema = schema, 
                    dframe = dframe, pgtraj = pgtraj, srid = srid))
    
    # Insert from temporary table into the schema
    test <- as_pgtraj(conn, schema = schema, srid = srid, db = FALSE)
    
    # Drop temporary table
    pgTrajDropTempT(conn, schema)
    
    # Insert CRS, comment and time zone on the pgtraj
    if (all(test)) {
        
        query <- paste0("UPDATE ",schema,".pgtrajs
                        SET proj4string = '", srs@projargs, "', 
                            \"comment\" = '", comment, "',
                            ltraj_tz = '",tz,"'
                        WHERE p_name = '", pgtraj, "';")
        
        query <- gsub(pattern = '\\s', replacement = " ", x = query)
        invisible(dbGetQuery(conn, query))
        message(paste0("The ltraj '", pgtraj, "' inserted into the database schema ", schema," successfully."))
        return(TRUE)
    } else {
        stop("Ltraj insert failed")
    }
}
