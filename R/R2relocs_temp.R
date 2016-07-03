#' Insert an ltraj data frame into the 'relocs_temp' table.
#' Input is an ltraj converted into a data frame with ld_opt()
#' The function does not preserve the row names of the ltraj, thus once relocations
#' are imported into the database, they cannot be linked back to the relocations 
#' in the original.
#' Missing relocations are ignored, because they cannot be cast into geometry
#' in the database.
#' 
###############################################################################
R2relocs_temp <- function(conn, schema, dframe, pgtraj, epsg = NULL) {
    # Set projection string if 
    if (epsg %% 1 == 0 & epsg > 0) {
        proj <- CRS(paste0("+init=epsg:", epsg))
    }
    DF <- dframe[complete.cases(dframe[,c("x", "y")]),]
    # Prepare the data frame to match 'relocs_temp'
    coords <- SpatialPoints(DF[, c("x", "y")], proj4string = proj)
    spdf <- SpatialPointsDataFrame(coords, DF)
    spdf <- spdf[,c("date", "id", "burst")]
    names(spdf) <- c("date", "a_name", "b_name")
    spdf$p_name <- pgtraj
    pgi <- pgInsertizeGeom(spdf, geom = "relocation", new.gid = "r_id")
    
    pgInsert(conn, pgi, c(schema, "relocs_temp"))
}