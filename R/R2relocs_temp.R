#' Insert an ltraj data frame into the 'relocs_temp' table.
#' Input is an ltraj converted into a data frame with ld_opt().
#' Ltraj row names are preserved. No transaction control.
#' 
#' @note ST_PointFromText() vs. ST_MakePoint() http://gis.stackexchange.com/a/122263/56083
#' 
#' @param conn Connection object created with RPostgreSQL
#' @param schema String. Name of the schema that stores or will store the pgtraj data model.
#' @param schema String. Name of the schema that stores or will store the pgtraj data model.
#' @param pgtraj String. Name of the new pgtraj. Defaults to the name of the 
#' variable that stores the ltraj.
#' @param epsg Numeric. The EPSG code of the Coordinate Reference System of the 
#' relocation coordinates in the ltraj. Defaults to 0.
#' 
#' TODO add Type I handling (deal with columns 'pkey' and the presence of 'date')
#' 
###############################################################################

data(ibex)
dframe <- ld_opt(ibex)

pgtraj <- "ibex"

ibexI <- typeII2typeI(ibex)
dframeI <- ld_opt(ibexI)
DF <- dframeI

make_relocs_temp(conn, "traj_t1")
make_pgtraj_schema(conn, "traj_t2")
make_relocs_temp(conn, "traj_t2")
make_pgtraj_schema(conn, "traj_t3")
make_relocs_temp(conn, "traj_t3")

drop_relocs_temp(conn, "traj_t2")


R2relocs_temp(conn, "traj_t1", dframe, pgtraj = "ibex", epsg = 0) # Type II
R2relocs_temp(conn, "traj_t2", dframeI, pgtraj = "ibex", epsg = 0) # Type I

R2relocs_temp <- function(conn, schema, dframe, pgtraj, epsg = NULL) {
    # Prepare the data frame to match 'relocs_temp'
    DF <- cbind(dframeI, "r_id" = row.names(dframeI))
    # TODO cannot create SpatialPoints form NAs, what now?
#    coords <- SpatialPoints(DF[, c("x", "y")], proj4string = srs)
#    spdf <- SpatialPointsDataFrame(coords, DF)
    # TODO add check for pkey column and then include it
    DF$p_name <- pgtraj
    i <- sapply(DF, is.factor)
    DF[i] <- lapply(DF[i], as.character)
    if ("pkey" %in% colnames(DF)) {
        DF <- DF[,c("x", "y", "id", "burst", "p_name", "r_id", "pkey")]
        names(DF) <- c("x", "y", "a_name", "b_name", "p_name", "r_id", "pkey")
    } else {
        DF <- DF[,c("x", "y", "date", "id", "burst", "p_name", "r_id")]
        names(DF) <- c("x", "y", "date", "a_name", "b_name", "p_name", "r_id")
        DF$date <- sapply(DF$date, 
                function(x) 
                    strftime(x, format = "%Y-%m-%d %H:%M:%S", tz = "", usetz = TRUE)
        )
    }
#    pgi <- pgInsertizeGeom(spdf, geom = "relocation")
#    pgInsert(conn, pgi, c(schema, "relocs_temp"))
    
    # Prepare column names for insert. X and Y columns must be DF[1:2].
    x <- colnames(DF)[3:length(DF)]
    x <- c("relocation", x)
    cols <- paste0('("', paste(x, collapse = '","'), '")')
    
    # Prepare values of the dataframe for insert
    parse_row <- function(x) {
        if (all(!is.na(x[1:2]))) {
            reloc <- paste0("ST_SetSRID(ST_MakePoint(", x["x"], ",", x["y"], "),", epsg, ")")
        } else {
            reloc <- "NULL"
        }
        paste0("(",
               reloc, ", '",
               toString(paste(x[3:length(x)], collapse = "','")),
               "')")
    }
    d1 <- apply(DF, 1, parse_row)
    values <- paste(d1, collapse = ",")
    query_insert <- paste("INSERT INTO relocs_temp ", cols, " VALUES ", values,";")
    
    # Begin transaction block and set database search path
    invisible(dbGetQuery(conn, "BEGIN TRANSACTION;"))
    query <- paste0("SET search_path TO ", schema, ",public;")
    invisible(dbGetQuery(conn, query))
    
    invisible(dbGetQuery(conn, query_insert))
    
    query <- "SET search_path TO \"$user\",public;"
    invisible(dbGetQuery(conn, query))
    invisible(dbCommit(conn))
    message(paste0("Data frame successfully inserted into ", schema,".relocs_temp"))
}