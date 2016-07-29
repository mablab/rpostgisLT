#' These functions meant to streamline the package development.
#' 
#' @author Balázs Dukai
#' 
###############################################################################

# Load packages
library(RPostgreSQL)
library(adehabitatLT)
library(rpostgisLT)


# Connect server
cs <- function() {
    drv <<- dbDriver("PostgreSQL")
    conn <<- dbConnect(drv, user="rpostgis", password="gsoc", dbname="rpostgis",
            host="basille-flrec.ad.ufl.edu")
    message("Connection established successfully")
}
# Reconnect server
rcs <- function() {
    dbDisconnect(conn)
    postgresqlCloseDriver(drv)
    dbUnloadDriver(drv)
    drv <<- dbDriver("PostgreSQL")
    conn <<- dbConnect(drv, user="rpostgis", password="gsoc", dbname="rpostgis",
            host="basille-flrec.ad.ufl.edu")
    message("Reconnected successfully")
}
