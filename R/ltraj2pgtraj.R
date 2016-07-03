
# TODO once SRID is stored in the ltraj, include that too

ltraj2pgtraj <- function(ltraj, conn, schema, pgtraj = NULL, epsg = NULL) {
    # 'pgtraj' defaults to the name of ltraj
    if (is.null(pgtraj)) {
        pgtraj <- deparse(substitute(ltraj))
    }
    
    # Convert ltraj to data frame
    dframe <- ld_opt(ltraj)
    
    # Import data frame into a temporary table
    make_relocs_temp(conn, schema)
    R2relocs_temp(conn, schema, dframe, pgtraj, epsg)
    
    # TODO Insert ltraj into the schema
    
    # Drop temporary table
    drop_relocs_temp(conn, schema)
}

#records <- ltraj2pgtraj(ibex)


