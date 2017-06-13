library(rpostgisLT)
#source("./utility/utility_functions.R")
library(lubridate)
library(sf)
library(shiny)
library(miniUI)
library(leaflet)

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

# if you are using the basille-flrec.ad.ufl.edu server, this is already prepared:
# pgtrajSchema(conn, schema = "roe_traj")
# 
# as_pgtraj(conn, relocations_table = c("example_data", "roe"), schema = "roe_traj",
#           pgtrajs = "study_area", animals = "animal_id", relocations = "geom",
#           timestamps = "acquisition_time", info_cols = c("sex", "age"),
#           rids = "loc_id")
# as_pgtraj(conn, relocations_table = c("example_data", "roe"), schema = "roe_traj",
#           pgtrajs = "both_area", animals = "animal_id", relocations = "geom",
#           timestamps = "acquisition_time", info_cols = c("sex", "age"),
#           rids = "loc_id")

# For the Shiny App the trajectories are directly pulled from the database and
# the app uses Leaflet to plot the trajectories on a base map.
# By using a reactive expression it is possible to interactively subset the
# *steps* based on their time stamp.

subsetSteps <- function(conn, schema, pgtraj, use_sf = TRUE) {
    # get trajectories from the database
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
    minT <- min(x$relocation_time)
    maxT <- max(x$relocation_time)
    bounds <- pgGetBoundary(conn, name = c(schema, view), geom = "step_geom")
    bounds <- spTransform(bounds, CRS("+init=epsg:4326"))
    
    ui <- bootstrapPage(
        
        tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
        
        leafletOutput("map", width = "100%", height = "100%"),
        
        absolutePanel(top = 10, right = 10,
                      sliderInput("range", "Timestamp", minT, maxT, 
                                  value = c(minT, maxT), step = 60
                                )
                    )
    )
    
    server <- function(input, output, session) {
        
        # reactivbe expression
        filteredData <- reactive({
            x[x$relocation_time >= input$range[1] & x$relocation_time <= input$range[2], ]
        })
        
        # Leaflet base map, and starting view centered at the trajectories
        output$map <- renderLeaflet({
            leaflet(x) %>%
                addTiles() %>% 
                fitBounds(lng1 = bbox(bounds)["x","min"], lat1 = bbox(bounds)["y","min"],
                          lng2 = bbox(bounds)["x","max"], lat2 = bbox(bounds)["y","min"])
        })
        
        # update plotted trajectories with the reactive expression output
        observe({
            leafletProxy("map", data = filteredData()) %>%
                clearShapes() %>%
                addPolylines(
                    fillOpacity = 1,
                    opacity = 1,
                    color = "#de2d26",
                    weight = 3
                )
        })
        
    }
    
    shinyApp(ui, server)
}

# Play around here, by loading different pgtrajes:
# pgtrajes in "roe_traj" schema are: "bondone", "rendena", "both_area"
# use_sf = TRUE – use simple feature geometry and st_read_db()
# use_sf = FALSE – use sp geometry and pgGetGeom()
subsetSteps(conn, "roe_traj", "bondone", use_sf = TRUE)

