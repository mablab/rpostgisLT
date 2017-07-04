library(sf)
library(lubridate)
library(shiny)
library(leaflet)
library(dplyr)
load("roe_sf.RData")

ltrajPlotter <- function(pgtraj_sf,
                         d_start,
                         t_start,
                         tzone,
                         increment,
                         interval) {
    # Start time
    t <- ymd_hms(paste(d_start, t_start), tz = tzone)
    t_next <- t + duration(hour = increment)
    
    st <-
        filter(pgtraj_sf,
               date >= t &
                   date < t + duration(hour = increment))
    st_next <-
        filter(
            pgtraj_sf,
            date >= t_next &
                date < t_next + duration(hour = increment)
        )
    
    factpal <- colorFactor("RdYlBu", st$id)
    
    ui <- bootstrapPage(
        tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
        h3(textOutput("tstamp")),
        h3(textOutput("window_end")),
        tags$script(
            '$(document).on("keydown",
            function (e) {
            if(e.which == 66) {
            Shiny.onInputChange("b", new Date());
            } else if (e.which == 78) {
            Shiny.onInputChange("n", new Date());
            }
            });
            '
    ),
    actionButton("b", "Back"),
    actionButton("n", "Next"),
    h5("press B or N"),
    leafletOutput("map", width = "100%", height = "100%")
    )
    
    server <- function(input, output) {
        x <- reactiveValues(currStep = st, nextStep = st_next)
        # get current time window and the next
        timeOut <- reactiveValues(currTime = t,
                                  nextTime = t + duration(hour = increment))
        
        # Only update timestamp on click
        observeEvent(input$n, {
            timeOut$currTime <- timeOut$currTime + duration(hour = increment)
            timeOut$nextTime <-
                timeOut$currTime + duration(hour = increment)
        })
        
        observeEvent(input$b, {
            timeOut$currTime <- timeOut$currTime - duration(hour = increment)
            timeOut$nextTime <-
                timeOut$currTime - duration(hour = increment)
        })
        
        # Report current timestamp
        output$tstamp <- renderText({
            paste("Time window start:",
                  format(timeOut$currTime, usetz = TRUE))
        })
        
        output$window_end <- renderText({
            paste("Time window end:",
                  format(timeOut$currTime + duration(hour = interval), usetz = TRUE))
        })

        
        # Leaflet base map, and starting view centered at the trajectories
        output$map <- renderLeaflet({
            map <- leaflet() %>%
                addTiles(group = "OSM (default)") %>%
                addPolylines(
                    data = st,
                    group = "trajfull",
                    fillOpacity = .5,
                    opacity = .5,
                    color = ~factpal(id),
                    # color = "red",
                    weight = 2
                ) %>%
                addLayersControl(overlayGroups = "OSM (default)",
                                 options = layersControlOptions(collapsed = FALSE))
        })
        
        # get the traj segment on every click, on either n or b
        observe({
            x$currStep <-
                filter(
                    pgtraj_sf,
                    date >= timeOut$currTime &
                        date < (timeOut$currTime + duration(hour = interval))
                )
            x$nextStep <-
                filter(
                    pgtraj_sf,
                    date >= timeOut$nextTime &
                        date < (timeOut$nextTime + duration(hour = interval))
                )
        })
        
        observe({
            leafletProxy("map", deferUntilFlush = TRUE) %>%
                clearGroup("traj") %>%
                addPolylines(
                    data = x$currStep,
                    group = "traj",
                    fillOpacity = 1,
                    opacity = 1,
                    color = ~ factpal(id),
                    # color = "red",
                    weight = 4
                )
        })
        
    }
    shinyApp(ui, server)
    
}


ltrajPlotter(
    pgtraj_sf = roe_sf,
    d_start = "2005-10-22",
    t_start = "00:00:00",
    tzone = "UTC",
    increment = 4,
    interval = 48
)

