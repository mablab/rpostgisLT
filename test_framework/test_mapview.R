library(mapview)
library(sf)
library(RPostgreSQL)
library(adehabitatLT)
library(rpostgisLT)


# DB connection -----------------------------------------------------------
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


# Mapview speed test ------------------------------------------------------
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


# Data for PoC ------------------------------------------------------------

data(ibex)
attr(ibex, "proj4string") <- CRS("+init=epsg:2154")
ltraj2pgtraj(conn, ibex, schema = "ibex", overwrite = TRUE)

# Window query ------------------------------------------------------------

get_t_window <- function(conn, schema, view, start_date, start_hour, interval){
    t_start <- paste(start_date, start_hour)
    t_interval <- paste(interval, "hour")
    sql_query <- paste0("
        SELECT
            a.step_id,
            a.step_geom,
            a.relocation_time,
            a.burst_name,
            a.animal_name,
            a.pgtraj_name
        FROM ", schema, ".", view, " a
        WHERE a.relocation_time >= '", t_start, "'::timestamp
        AND a.relocation_time < ('", t_start, "'::timestamp + '",
                        t_interval, "'::INTERVAL)
        AND a.step_geom IS NOT NULL;")
    # s <- gsub("\n", "", sql_query)
    # print(s)
    return(st_read_db(conn, query=sql_query, geom_column = "step_geom"))
    # return(pgGetGeom(conn, query = sql_query))
}

x <- get_t_window(conn, "ibex", "step_geometry_ibex", "2003-06-01", "00:00:00", 24)

# Mapview -----------------------------------------------------------------

traj_mapview_window <- function(conn, schema, pgtraj, increment){
    view <- paste0("step_geometry_", pgtraj)
    # 
    # ANSWER <- readline("step next (n) or back (b)?")
    # if(ANSWER=="n")
    x <- get_t_window(conn, schema, view, "2003-06-01", "00:00:00", 24)
    # x <- st_transform(x, "+init=epsg:4326")
    mapview(x, native.crs = FALSE, map.types = "OpenTopoMap")
}

traj_mapview_window(conn, "ibex", "ibex")

cat("What's your name? ")
x <- readLines(file("stdin"),1)
print(x)
