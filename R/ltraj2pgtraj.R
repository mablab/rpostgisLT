#' Import an ltraj object from R into a traj schema.
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
#' @export 
#' 
#' @example
#' ltraj2pgtraj()
#' 
# TODO once SRID is stored in the ltraj, include that too
################################################################################
ltraj2pgtraj <- function(ltraj, conn, schema, pgtraj = NULL, epsg = NULL,
        comment = NULL) {
    # 'pgtraj' defaults to the name of ltraj
    if (is.null(pgtraj)) {
        pgtraj <- deparse(substitute(ltraj))
    }
    
    # Set projection
    if (is.null(epsg)) {
        epsg <- 0
    } else if (!is.numeric(epsg)) {
        stop("EPSG code must be numeric")
    }
    
    # Convert ltraj to data frame
    dframe <- ld_opt(ltraj)
    
    # Check and create a pgtraj schema
    x <- make_pgtraj_schema(conn, schema)
    # If schema doesn't exists and user doesn't want to create it
    if (x == 0) {
        stop("Returning from function...")
    }
    
    # Import data frame into a temporary table
    make_relocs_temp(conn, schema)
    R2relocs_temp(conn, schema, dframe, pgtraj, epsg)
    
    # Insert from temporary table into the schema
    as_pgtraj(conn, schema, db = FALSE)
    
    # Drop temporary table
    drop_relocs_temp(conn, schema)
    
    # Insert comment on the pgtraj
    # INSERT INTO pgtrajs (comment) VALUES () WHERE p_name LIKE pgtraj
    
}

#records <- ltraj2pgtraj(ibex)


