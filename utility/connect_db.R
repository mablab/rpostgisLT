#' These functions meant to streamline the package development.
#' 
#' @author Balázs Dukai
#' 
###############################################################################

# Connect to DB -----------------------------------------------------------

library(RPostgreSQL)

cs <- function(pw, drv = "PostgreSQL", host = "ufl", user="rpostgis",
               dbname="rpostgis") {
    if (host == "local") {
        conn <- dbConnect(drv, user=user, password=pw, dbname=dbname,
                          host="localhost")
        message(paste("Connection to host",host,"established successfully"))
        return(conn)
    } else if (host == "ufl") {
        conn <- dbConnect(drv, user=user, password=pw, dbname="rpostgis",
                          host="basille-flrec.ad.ufl.edu")
        message(paste("Connection to host",host,"established successfully"))
        return(conn)
    }
}
