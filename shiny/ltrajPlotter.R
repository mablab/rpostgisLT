library(sf)
library(lubridate)
library(shiny)
library(leaflet)
library(dplyr)

ltrajPlotter <- function(pgtraj_sf,
                         d_start,
                         t_start,
                         tzone,
                         increment,
                         interval) {
    # Start time
    t <- ymd_hms(paste(d_start, t_start), tz = tzone)
    
    st <-
        filter(pgtraj_sf,
               date >= t &
                   date < t + duration(hour = increment))
    
    factpal <- colorFactor("RdYlBu", st$id)
    
    ui <- bootstrapPage(
        tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
        h4(textOutput("tstamp")),
        tags$script(
            '$(document).on("keydown",
            function (e) {
            var d = new Date();
            if(e.which == 66) {
            Shiny.onInputChange("b", Math.round((d.getMilliseconds()+251) / 500) );
            } else if (e.which == 78) {
            Shiny.onInputChange("n", Math.round((d.getMilliseconds()+251) / 500) );
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
        x <- reactiveValues(currStep = st, counter = 0)
        # get current time window and the next
        timeOut <- reactiveValues(currTime = t)
        
        # Only update timestamp on click
        observeEvent(input$n, {
            # for assigning alternating group names
            x$counter <- x$counter + 1
            timeOut$currTime <-
                timeOut$currTime + duration(hour = increment)
            x$currStep <-
                filter(pgtraj_sf,
                       date >= timeOut$currTime &
                           date < (timeOut$currTime + duration(hour = interval)))
        })
        
        observeEvent(input$b, {
            # for assigning alternating group names
            x$counter <- x$counter + 1
            timeOut$currTime <-
                timeOut$currTime - duration(hour = increment)
            x$currStep <-
                filter(pgtraj_sf,
                       date >= timeOut$currTime &
                           date < (timeOut$currTime + duration(hour = interval)))
        })
        
        # Report current timestamp
        output$tstamp <- renderText({
            paste(
                "Time window:",
                format(timeOut$currTime, usetz = TRUE),
                "—",
                format(
                    timeOut$currTime + duration(hour = interval),
                    usetz = TRUE
                )
            )
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
                    color = ~ factpal(id),
                    # color = "red",
                    weight = 2
                ) %>%
                addLayersControl(overlayGroups = "OSM (default)",
                                 options = layersControlOptions(collapsed = FALSE))
        })
        
        observe({
            if (x$counter %% 2 == 0) {
                gname <- "traj"
            } else {
                gname <- "trajnew"
            }
            proxy <- leafletProxy("map", data = x$currStep) %>%
                addPolylines(
                    group = gname,
                    fillOpacity = 1,
                    opacity = 1,
                    color = ~ factpal(id),
                    weight = 4
                )
            if (x$counter %% 2 == 0) {
                proxy %>% clearGroup("trajnew")
            } else {
                proxy %>% clearGroup("traj")
            }
        })
        
    }
    shinyApp(ui, server)
    
    }
