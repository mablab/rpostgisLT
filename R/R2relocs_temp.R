#' Insert an ltraj data frame into the 'relocs_temp' table.
#' Input is an ltraj converted into a data frame with ld_opt()
#' 
###############################################################################

library(sp)

data(ibex)
dframe <- ld_opt(ibex)
pgtraj <-  "ibex"
schema <- "traj_t4" 
epsg <- 4326
epsg <- 0
epgs <- NULL

R2relocs_temp <- function(conn, schema, dframe, pgtraj, epsg = NULL) {
    # Set projection string if 
    if (epsg %% 1 == 0 & epsg > 0) {
        proj <- CRS(paste0("+init=epsg:", epsg))
    }
    # Missing relocations are ignored, because they cannot be cast into geometry
    DF <- na.omit(dframe)
    # Prepare the data frame to match 'relocs_temp'
    DF <- cbind(DF, "r_id" = paste0("nextval('", schema, ".temp_r_id_seq')"))
    coords <- SpatialPoints(DF[, c("x", "y")], proj4string = proj)
    spdf <- SpatialPointsDataFrame(coords, DF)
    spdf <- spdf[,c("date", "id", "burst", "r_id")]
    names(spdf) <- c("date", "a_name", "b_name", "r_id")
    spdf$p_name <- pgtraj
    pgi <- pgInsertizeGeom(spdf, geom = "relocation")
    
    pgInsert(conn, pgi, c(schema, "relocs_temp"))
}