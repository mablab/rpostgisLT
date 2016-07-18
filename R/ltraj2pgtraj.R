#' Export an ltraj object from R into a traj database schema.
#' 
#' @description 
#' \code{ltraj2pgtraj} creates a new traj schema or uses an existing one and 
#' exports an ltraj to the database. Uses \code{as_pgtraj} to insert the values 
#' into the traj schema.
#' 
#' @author Balázs Dukai \email{balazs.dukai@@gmail.com}
#' 
#' @param ltraj An ltraj object.
#' @param conn Connection object created with RPostgreSQL
#' @param schema String. Name of the schema that stores or will store the pgtraj data model.
#' @param pgtraj String. Name of the new pgtraj. Defaults to the name of the 
#' variable that stores the ltraj.
#' @param epsg Numeric. The EPSG code of the Coordinate Reference System of the 
#' relocation coordinates in the ltraj. Defaults to 0.
#' @param comment String. A comment that will be stored with the pgtraj in the database.
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
ltraj2pgtraj <- function(conn, ltraj, schema = "traj", pgtraj = NULL, epsg = NULL,
        comment = NULL) {
    # 'pgtraj' defaults to the name of ltraj
    if (is.null(pgtraj)) {
        pgtraj <- deparse(substitute(ltraj))
    }
    
    # Set projection
    if (is.null(epsg)) {
        epsg <- 0
        srs <- sp::CRS()@projargs
    } else if (!is.numeric(epsg)) {
        stop("EPSG code must be numeric")
    } else {
        srs <- sp::CRS(paste0("+init=epsg:", epsg))@projargs
    }
    
    # Convert ltraj to data frame
    dframe <- ld_opt(ltraj)
    
    # Check and create a pgtraj schema
    x <- make_pgtraj_schema(conn, schema)
    # If schema doesn't exists and user doesn't want to create it
    if (x == "Exit") {
        stop("Returning from function...")
    }
    
    # Import data frame into a temporary table
    make_relocs_temp(conn, schema)
    test <- suppressMessages(R2relocs_temp(conn, schema, dframe, pgtraj, epsg))
    
    # Insert from temporary table into the schema
    test <- as_pgtraj(conn, schema, db = FALSE)
    
    # Drop temporary table
    drop_relocs_temp(conn, schema)
    
    # Insert CRS and comment on the pgtraj
    if (all(test)) {
        
        query <- paste0("UPDATE ",schema,".pgtrajs
                        SET r_proj = '", srs, "', \"comment\" = '", comment, "'
                        WHERE p_name = '", pgtraj, "';")
#        query <- paste0("UPDATE pgtrajs (r_proj, comment) 
#                        VALUES ('", srs, "', '", comment, "')
#                        WHERE p_name = '", pgtraj, "';")
        query <- gsub(pattern = '\\s', replacement = " ", x = query)
        invisible(RPostgreSQL::dbGetQuery(conn, query))
        message(paste0("The ltraj '", pgtraj, "' inserted into the database schema ", schema," successfully."))
        return(TRUE)
    } else {
        stop("Ltraj insert failed")
    }
}
