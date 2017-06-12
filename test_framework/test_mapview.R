# Libraries ---------------------------------------------------------------

library(mapview)
library(sf)
library(RPostgreSQL)
library(adehabitatLT)
library(rpostgisLT)
library(lubridate)
library(shiny)

# DB connection -----------------------------------------------------------
# NOTE: need to connect to a database on your own

# Connect server
# cs <- function(pw, drv = "PostgreSQL", host = "local") {
#     if (host == "local") {
#         conn <<- dbConnect(drv, user="bdukai", password=pw, dbname="rpostgisLT",
#                            host="localhost")
#         message(paste("Connection to host",host,"established successfully"))
#     } else if (host == "ufl") {
#         conn <<- dbConnect(drv, user="rpostgis", password=pw, dbname="rpostgis",
#                            host="basille-flrec.ad.ufl.edu")
#         message(paste("Connection to host",host,"established successfully"))
#     }
# }
# # Reconnect server
# rcs <- function(pw, drv = "PostgreSQL") {
#     dbDisconnect(conn)
#     postgresqlCloseDriver(drv)
#     dbUnloadDriver(drv)
#     cs()
# }
# 
# drv <- "PostgreSQL"
# cs(pw="gsoc", drv = drv, host = "ufl")

# Data for PoC ------------------------------------------------------------

# data(ibex)
# attr(ibex, "proj4string") <- CRS("+init=epsg:2154") # my best guess for the Ibex CRS
# ltraj2pgtraj(conn, ibex, schema = "ibex", overwrite = TRUE)
# tzone <- tz(ibex[[1]][1, "date"]) # because I'm lazy to pull from DB

# as_pgtraj(conn, relocations_table = c("example_data", "stork_gps"),
#           schema = "stork_traj", pgtrajs = "year", animals = "animal_id",
#           relocations = "geom", timestamps = "acquisition_time",
#           rids = "gps_data_animals_id")

# Window query ------------------------------------------------------------

get_t_window <- function(conn, schema, view, time, interval){
    # t_start <- paste(start_date, start_hour)
    t <- format(time, usetz = TRUE)
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
        WHERE a.relocation_time >= '", t, "'::timestamptz
        AND a.relocation_time < ('", t, "'::timestamptz + '",
                        t_interval, "'::INTERVAL)
        AND a.step_geom IS NOT NULL;")
    # s <- gsub("\n", "", sql_query)
    # print(s)
    return(st_read_db(conn, query=sql_query, geom_column = "step_geom"))
    # return(pgGetGeom(conn, query = sql_query))
}

get_full_traj <- function(conn, schema, view){
    # t_start <- paste(start_date, start_hour)
    sql_query <- paste0("
        SELECT
            a.step_id,
            a.step_geom,
            a.relocation_time,
            a.burst_name,
            a.animal_name,
            a.pgtraj_name
        FROM ", schema, ".", view, " a
        WHERE a.step_geom IS NOT NULL;")
    # s <- gsub("\n", "", sql_query)
    # print(s)
    return(st_read_db(conn, query=sql_query, geom_column = "step_geom"))
    # return(pgGetGeom(conn, query = sql_query))
}

# d_start <- "2003-06-01"
# t_start <- "00:00:00"
# t <- ymd_hms(paste(d_start, t_start), tz = tzone)
# x <- get_t_window(conn, "ibex", "step_geometry_ibex", t, 24)

# Mapview -----------------------------------------------------------------

traj_mapview_window <- function(conn, schema, pgtraj, d_start, t_start, tzone,
                                increment, nr_increment, interval,
                                print_map = TRUE, basemap = TRUE,
                                collect_features = TRUE, sleep){
    view <- paste0("step_geometry_", pgtraj)
    # Start time
    t <- ymd_hms(paste(d_start, t_start), tz = tzone)
    # Prepare map output
    if(print_map){
        # Get first set of features
        x <- get_t_window(conn, schema, view, t, interval)
        # In case features are collected and not replaced on the map
        if(basemap){
            mtype <- "OpenTopoMap"
            cr <- FALSE
        } else {
            mtype <- NA
            cr <- TRUE
        }
        # prepare map
        m <- mapview(x, native.crs = cr, map.types = mtype, zcol = "animal_name")
    }
    # Simulate stepping through a trajectory
    for(i in 1:nr_increment){
        # Wait until next "click"
        Sys.sleep(sleep)
        # Get new set of features in time window
        t <- t + duration(hour = increment)
        x <- get_t_window(conn, schema, view, t, interval)
        # x <- st_transform(x, "+init=epsg:4326")
        if(print_map) {
            if(collect_features){
                suppressWarnings(m <- m + mapview(x, zcol = "animal_name"))
            } else {
                # suppressWarnings(m <- mapview(x, native.crs = cr,
                #                               map.types = mtype,
                #                               zcol = "animal_name"))
                m@map %>% addPolylines(data = x)
            }
            print(m)
        } else {
            print(head(x))
        }
    }
    # x <- get_t_window(conn, schema, view, "2003-06-01", "00:00:00", increment)
    # # x <- st_transform(x, "+init=epsg:4326")
    # mapview(x, native.crs = FALSE, map.types = "OpenTopoMap")
}

# Settings and run ---------------------------------------------------------

# Ibex
schema <- "ibex"
pgtraj <- "ibex"
d_start <- "2003-06-01" # first date
t_start <- "00:00:00" # first hour
# tzone <- tzone # time zone for time input
tzone <- "Europe/Amsterdam"
increment <- 4 # increment by 1 hours at a time
nr_increment <- 10 # increment 10x
interval <- 24 # hours of time window to loa

# Storks
schema <- "stork_traj"
pgtraj <- "2004"
d_start <- "2004-06-01" # first date
t_start <- "00:00:00" # first hour
# tzone <- tzone # time zone for time input
tzone <- "Europe/Amsterdam"
increment <- 24 # increment by 1 hours at a time
nr_increment <- 10 # increment 10x
interval <- 48 # hours of time window to load

# sleep <- 0.5 # seconds until next query
# print_map – plot trajectories or only print data in console?
# basemap – use a basemap (WMS) that also requires reprojection,
#           or don't reproject and don't use basemap (faster)
# collect_features – collect the queried trajectory segments on the map, or
#                   only show the current segments

# traj_mapview_window(conn, schema, pgtraj, d_start, t_start, tzone, increment,
#                     nr_increment, interval, print_map = TRUE, basemap = TRUE,
#                     collect_features = FALSE, sleep)

# still creates new plots
# m <- mapview(gadmCHE)
# m@map %>% addCircleMarkers(data = breweries91)



# Shiny integration -------------------------------------------------------

# Need to create a new View in DB with EPSG:4326, because that's what Leaflet
# understands by default. Coordinate transformation can be expensive.

source("./test_framework/makeShinyView.R")
makeShinyView(conn, schema, pgtraj)

incrementSteps <- function(conn, schema, pgtraj, d_start, t_start, tzone, increment,
                        nr_increment, interval) {
    view <- paste0("step_geometry_shiny_", pgtraj)
    # Start time
    t <- ymd_hms(paste(d_start, t_start), tz = tzone)
    # Get initial set of trajectories
    st <- get_t_window(conn, schema, view, t, interval)
    
    # Get full traj
    st.1 <- get_full_traj(conn, schema, view)
    
    factpal <- colorFactor(topo.colors(4), st$animal_name)

    ui <- bootstrapPage(
        tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
        h3(textOutput("tstamp")),
        tags$script('$(document).on("keydown",
                 function (e) {
                 if(e.which == 66) {
                   Shiny.onInputChange("b", new Date());
                 } else if (e.which == 78) {
                   Shiny.onInputChange("n", new Date());
                 }
                 });
                '),
        actionButton("b", "Back"),
        actionButton("n", "Next"),
        h5("press B or N"),
        leafletOutput("map", width = "100%", height = "100%")
    )
    
    server <- function(input, output) {
        
        w <- reactiveValues(data = st.1)
        x <- reactiveValues(data = st)
        timeOut <- reactiveValues(data = t)
        
        observeEvent(input$n, {
            timeOut$data <- timeOut$data + duration(hour = increment)
            x$data <- get_t_window(conn, schema, view, timeOut$data, interval)
        })
        
        observeEvent(input$b, {
            timeOut$data <- timeOut$data - duration(hour = increment)
            x$data <- get_t_window(conn, schema, view, timeOut$data, interval)
        })
        
        # Report current timestamp
        output$tstamp <- renderText({
            paste("Current time stamp:", format(timeOut$data, usetz = TRUE))
        })

        # Leaflet base map, and starting view centered at the trajectories
        output$map <- renderLeaflet({
            if (is.null(w$data)) return()
            leaflet(w$data) %>%
                addTiles() %>%
                addPolylines(
                    group = "trajfull",
                    fillOpacity = .8,
                    opacity = .8,
                    color = ~factpal(animal_name),
                    weight = 2
                ) %>%
                addLayersControl(
                    overlayGroups = "trajfull",
                    options = layersControlOptions(collapsed = FALSE)
                )
        })
        
        observe({
            leafletProxy("map", data = x$data) %>%
                clearGroup("traj") %>%
                addPolylines(
                    group = "traj",
                    fillOpacity = 1,
                    opacity = 1,
                    color = ~factpal(animal_name),
                    weight = 4
                )
        })
    }
    shinyApp(ui, server)
}

incrementSteps(conn, schema, pgtraj, d_start, t_start, tzone, increment,
            nr_increment, interval)

