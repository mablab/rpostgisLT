# pgtraj2ltraj

#' Import a pgtraj into an ltraj.
#'
#' \code{pgtraj2ltraj} imports a single pgtraj from a database into an
#' ltraj object.
#'
#' @param conn Connection object created with RPostgreSQL
#' @param pgtraj String. Name of the pgtraj
#' @param schema String. Name of the schema storing the pgtraj
#' @return an ltraj object
#' @importFrom stats complete.cases
#' @importFrom sp CRS
#' @aliases readTraj
#' @author Balázs Dukai \email{balazs.dukai@@gmail.com}
#' @export
#' @examples
#' \dontrun{
#'  # create ltraj from pgtraj named "ibex" in schema "traj_t2"
#'  ibex<-pgtraj2ltraj(conn, "ibex", "traj_t2")
#' }

pgtraj2ltraj <- function(conn, pgtraj, schema = "traj") {
    ## check PostgreSQL connection
    dbConnCheck(conn)
    # sanitize schema name
    schema_q <- dbQuoteIdentifier(conn, schema)
    
    view <- paste0("parameters_", pgtraj)
    view_q <- dbQuoteIdentifier(conn, view)
    
    # check if infolocs exist
    info <- NULL
    if (dbExistsTable(conn, c(schema, paste0("infolocs_", pgtraj)))) {
        # check for column names
        info_info <-
            dbTableInfo(conn, c(schema, paste0("infolocs_",
                                               pgtraj)))$column_name
        if (length(info_info) > 1 && "step_id" %in% info_info) {
            info <- getPgtrajWithInfo(conn, pgtraj, schema)
        }
    }
    
    # get ltraj data from parameters view
    DF <- invisible(dbGetQuery(conn, paste0(
        "SELECT * FROM ",
        schema_q, ".", view_q, ";"
    )))
    # remove step_id column
    DF$step_id <- NULL
    
    # Get time zone
    sql_query <-
        paste0(
            "SELECT time_zone FROM ",
            schema_q,
            ".pgtraj WHERE pgtraj_name = ",
            dbQuoteString(conn, pgtraj),
            ";"
        )
    tz <- dbGetQuery(conn, sql_query)[1, 1]
    
    # Get proj4string
    sql_query <- paste0(
        "SELECT proj4string FROM ",
        schema_q,
        ".pgtraj WHERE pgtraj_name = ",
        dbQuoteString(conn, pgtraj),
        ";"
    )
    proj4string <- dbGetQuery(conn, sql_query)[1, 1]
    
    # Rename and prepare data frame for conversion to ltraj
    names(DF)[names(DF) == "r2n"] <- "R2n"
    names(DF)[names(DF) == "abs_angle"] <- "abs.angle"
    names(DF)[names(DF) == "rel_angle"] <- "rel.angle"
    names(DF)[names(DF) == "animal_name"] <- "id"
    
    DF <- DF[,-which(names(DF) == "pgtraj")]
    
    # TYPE I – insert 1 for 'dt'
    if (all(is.na(DF$date))) {
        k <- table(DF$burst)
        for (i in names(k)) {
            len <- as.vector(k[i]) - 1
            DF[DF$burst == i, "dt"][1:len] <- 1
        }
    }
    
    # Check if the row names are stored in the pgtraj
    rnames <- all(stats::complete.cases(DF$r_rowname))
    if (rnames) {
        names(DF)[names(DF) == "r_rowname"] <- "r.row.names"
        # TYPE I (date = r.row.names column)
        if (all(is.na(DF$date))) {
            DF$date <- as.integer(DF$r.row.names)
        }
    } else {
        DF <- DF[,-which(names(DF) == "r_rowname")]
        # TYPE I (date = row.names(DF))
        if (all(is.na(DF$date))) {
            DF$date <- as.integer(row.names(DF))
        }
    }
    
    
    # Set time zone
    if (!class(DF$date)[1] == "integer") {
        attr(DF$date, "tzone") <- tz
    }
    
    # Cast into ltraj
    ltraj <- dl_opt(DF, rnames)
    
    # attach infolocs if exist
    if (!is.null(info)) {
        for (i in 1:length(ltraj)) {
            # match infolocs rownames to ltraj rownames
            row.names(info[[i]]) <- row.names(ltraj[[i]])
        }
        infolocs(ltraj) <- info
    }
    
    if (proj4string %in% c("NA", "NULL", "NaN")) {
        attr(ltraj, "proj4string") <- sp::CRS()
    } else {
        attr(ltraj, "proj4string") <- sp::CRS(proj4string)
    }
    
    message(paste0("Ltraj successfully created from ", pgtraj,
                   "."))
    
    return(ltraj)
}