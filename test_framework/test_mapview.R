library(mapview)
library(sf)
library(RPostgreSQL)

# NOTE: need to connect to a database on your own
# conn <- ?
# Connect server
cs <- function(pw, drv = "PostgreSQL", host = "local") {
    if (host == "local") {
        conn <<- dbConnect(drv, user="bdukai", password=pw, dbname="rpostgisLT",
                           host="localhost")
        message(paste("Connection to host",host,"established successfully"))
    } else if (host == "ufl") {
        conn <<- dbConnect(drv, user="rpostgis", password=pw, dbname="rpostgis",
                           host="basille-flrec.ad.ufl.edu")
        message(paste("Connection to host",host,"established successfully"))
    }
}
# Reconnect server
rcs <- function(pw, drv = "PostgreSQL") {
    dbDisconnect(conn)
    postgresqlCloseDriver(drv)
    dbUnloadDriver(drv)
    cs()
}

drv <- "PostgreSQL"
cs(pw="gsoc", drv = drv, host = "ufl")


traj_mapview <- function(conn, schema, pgtraj, use_sf = TRUE){
    view <- paste0("step_geometry_", pgtraj)
    SQL <- paste0("select step_id, step_geom, relocation_time
                  from ", schema, ".", view,
                  " where step_geom is not null")
    if (use_sf == TRUE) {
        x <- suppressWarnings(st_read_db(conn, query = SQL))
        x <- st_transform(x, "+init=epsg:4326")
    } else {
        x <- pgGetGeom(conn, geom = "step_geom", query = SQL)
        x <- spTransform(x, CRS("+init=epsg:4326"))
    }
    mapview(x, native.crs = FALSE, map.types = "OpenTopoMap")
}

# Play around here, by loading different pgtrajes:
# pgtrajes in "roe_traj" schema are: "bondone", "rendena", "both_area"
# use_sf = TRUE – use simple feature geometry and st_read_db()
# use_sf = FALSE – use sp geometry and pgGetGeom()
traj_mapview(conn, "roe_traj", "bondone", use_sf = TRUE)

