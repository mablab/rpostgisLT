# Libraries ---------------------------------------------------------------

library(sf)
library(RPostgreSQL)
library(adehabitatLT)
library(rpostgisLT)
library(lubridate)
library(shiny)
library(leaflet)

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

# Ibex
#
# data(ibex)
# attr(ibex, "proj4string") <- CRS("+init=epsg:27572") # my best guess for the Ibex CRS
# ltraj2pgtraj(conn, ibex, schema = "ibex_traj", overwrite = TRUE)
# tzone <- tz(ibex[[1]][1, "date"]) # because I'm lazy to pull from DB

# Storks
# need to create column "year" in "strok_gps" first
#
# as_pgtraj(conn, relocations_table = c("example_data", "stork_gps"),
#           schema = "stork_traj", pgtrajs = "year", animals = "animal_id",
#           relocations = "geom", timestamps = "acquisition_time",
#           rids = "gps_data_animals_id")

# Settings -----------------------------------------------------------------

# Ibex
schema <- "ibex_traj"
pgtraj <- "ibex"
d_start <- "2003-06-01" # first date
t_start <- "00:00:00" # first hour
tzone <- "Europe/Paris"
increment <- 4 # increment by 1 hours at a time
nr_increment <- 10 # increment 10x
interval <- 24 # hours of time window to load

# Storks
schema <- "stork_traj"
pgtraj <- "2004"
d_start <- "2004-06-01" # first date
t_start <- "00:00:00" # first hour
tzone <- "Europe/Amsterdam"
increment <- 24 # increment by 1 hours at a time
nr_increment <- 10 # increment 10x
interval <- 48 # hours of time window to load

# Need to create a new View in DB with EPSG:4326, because that's what Leaflet
# understands by default. Coordinate transformation can be expensive.
source("./test_framework/makeShinyView.R")
# keep in mind that the current version of makeShinyView materializes the views
makeShinyView(conn, schema, pgtraj)

# Queriess ------------------------------------------------------------

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
            st_makeline(step_geom)::geometry(linestring, 4326) AS traj_geom,
            animal_name
        FROM ", schema, ".", view, "
        GROUP BY animal_name;")
    # s <- gsub("\n", "", sql_query)
    # print(s)
    return(st_read_db(conn, query=sql_query, geom_column = "traj_geom"))
    # return(pgGetGeom(conn, query = sql_query))
}

# Shiny App----------------------------------------------------------------

view <- "step_geometry_shiny_2004"

incrementSteps <- function(conn, schema, pgtraj, d_start, t_start, tzone, increment,
                        nr_increment, interval) {
    view <- paste0("step_geometry_shiny_", pgtraj)
    # Start time
    t <- ymd_hms(paste(d_start, t_start), tz = tzone)
    t_next <- t + duration(hour = increment)
    # Get initial set of trajectories
    st <- get_t_window(conn, schema, view, t, interval)
    st_next <- get_t_window(conn, schema, view, t_next, interval)
    
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
        x <- reactiveValues(currStep = st, nextStep = st_next)
        # get current time window and the next
        timeOut <- reactiveValues(currTime = t,
                                  nextTime = t + duration(hour = increment))
        
        # Only update timestamp on click
        observeEvent(input$n, {
            timeOut$currTime <- timeOut$currTime + duration(hour = increment)
            timeOut$nextTime <- timeOut$currTime + duration(hour = increment)
        })
        
        observeEvent(input$b, {
            timeOut$currTime <- timeOut$currTime - duration(hour = increment)
            timeOut$nextTime <- timeOut$currTime - duration(hour = increment)
        })
        
        # Report current timestamp
        output$tstamp <- renderText({
            paste("Current time stamp:", format(timeOut$currTime, usetz = TRUE))
        })

        # Leaflet base map, and starting view centered at the trajectories
        output$map <- renderLeaflet({
            if (is.null(w$data)) {
                return()
            } else {
                map <- leaflet() %>%
                    addTiles(group = "OSM (default)") %>%
                    addPolylines(
                        data = w$data,
                        group = "trajfull",
                        fillOpacity = .5,
                        opacity = .5,
                        color = ~factpal(animal_name),
                        weight = 2
                    ) %>%
                    addLayersControl(
                        overlayGroups = c("OSM (default)", "trajfull"),
                        options = layersControlOptions(collapsed = FALSE)
                    ) 
                
                # map %>% 
                #     addPolylines(
                #         data = x$currStep,
                #         group = "traj",
                #         fillOpacity = 1,
                #         opacity = 1,
                #         color = ~factpal(animal_name),
                #         weight = 4
                #     )
            }
        })
        
        # get the traj segment on every click, on either n or b
        observe({
            x$currStep <- get_t_window(conn, schema, view, timeOut$currTime, interval)
            x$nextStep <- get_t_window(conn, schema, view, timeOut$nextTime, interval)
        })
        
        observe({
            leafletProxy("map", data = x$currStep, deferUntilFlush = TRUE) %>%
                clearGroup("traj") %>% 
                addPolylines(
                    data = x$currStep,
                    group = "traj",
                    fillOpacity = 1,
                    opacity = 1,
                    color = ~factpal(animal_name),
                    weight = 4
                ) %>% 
                addPolylines(
                    data = x$nextStep,
                    group = "trajnew",
                    fillOpacity = 1,
                    opacity = 1,
                    color = ~factpal(animal_name),
                    weight = 4
                ) %>% 
                clearGroup("trajnew")
        })
    
    }
    shinyApp(ui, server)
}


# Run ---------------------------------------------------------------------

incrementSteps(conn, schema, pgtraj, d_start, t_start, tzone, increment,
                nr_increment, interval)

