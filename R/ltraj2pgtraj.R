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
################################################################################
ltraj2pgtraj <- function(conn, ltraj, schema = "traj", pgtraj = NULL, 
        comment = NULL, create = FALSE, new.srid = NULL) {
    # 'pgtraj' defaults to the name of ltraj
    if (is.null(pgtraj)) {
        pgtraj <- deparse(substitute(ltraj))
    }
    # FIXME pgtraj can only contain DB table-name-proof characters, include in test_input()

    # Set projection
    srs <- attr(ltraj, "proj4string")

    if (is.null(srs)) {
        srid <- 0
    } else {
        srid <- pgSRID(conn = conn, crs = srs,create = create, new.srid = new.srid)
        srs <- srs@projargs
    }
    
    # Get time zone
    tz <- attr(ltraj[[1]]$date, "tzone")
    
    # Convert ltraj to data frame
    dframe <- ld_opt(ltraj)
    
    # Check and create a pgtraj schema
    # pgTrajSchema() has its own transaction contral
    x <- pgTrajSchema(conn, schema)
    # If schema creation unsuccessful
    if (!isTRUE(x)) {
        stop("Traj schema couln't be created, returning from function.")
    }

    # Begin transaction block
    invisible(dbSendQuery(conn, "BEGIN TRANSACTION;"))
    
    # Import data frame into a temporary table
    res <- tryCatch({
                
                pgTrajTempT(conn, schema)
                
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

    res2 <- tryCatch({
                
                pgTrajR2TempT(conn, schema = schema, 
                                dframe = dframe, pgtraj = pgtraj, srid = srid)
                
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

    # Insert from temporary table into the schema
    res3 <- tryCatch({
                
                as_pgtraj(conn, schema = schema, srid = srid, db = FALSE)
                
            }, warning = function(x) {
                
                message(x)
                message(" . Rolling back transaction")
                dbRollback(conn) # needs to be here because tryCatch() in 
                # as_pgtraj() only evaluates withing its own scope and does
                # not pass the dbRollback() to ltraj2pgtraj().
                stop("Returning from function")
                
            }, error = function(x) {
                
                message(x)
                message(". Rolling back transaction")
                dbRollback(conn)
                stop("Returning from function")
                
            })
    
    res <- c(res, res3)

    # Insert CRS, comment and time zone on the pgtraj
    if (all(res)) {
        
        query <- paste0("UPDATE ",schema,".pgtraj
                        SET proj4string = '", srs, "', 
                            \"comment\" = '", comment, "',
                            time_zone = '",tz,"'
                        WHERE pgtraj_name = '", pgtraj, "';")
        
        query <- gsub(pattern = '\\s', replacement = " ", x = query)
        invisible(dbSendQuery(conn, query))
        
        dbCommit(conn)
        
        message(paste0("The ltraj '", pgtraj, "' successfully inserted into the database schema '", schema,"'."))
        return(TRUE)
        
    } else {
        dbRollback(conn)
        stop("Ltraj insert failed")
    }
}
