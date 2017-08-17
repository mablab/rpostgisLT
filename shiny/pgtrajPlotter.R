# library(sf)
library(lubridate)
library(shiny)
library(leaflet)
# library(dplyr)
# library(DBI)
library(htmltools)
library(mapview)
library(shinyWidgets)
library(testthat)

source("./shiny/utils.R")
source("./shiny/createShinyView.R")


# Shiny App----------------------------------------------------------------


pgtrajPlotter <-
    function(conn,
             schema,
             pgtraj,
             layers_vector=NULL,
             layers_params_vector=NULL,
             layer_raster=NULL,
             layers_params_raster=NULL) {
        view <- paste0("step_geometry_shiny_", pgtraj)
        # Get default time parameters
        time_params <- get_traj_defaults(conn, schema, view, pgtraj)
        
        tzone <- time_params$time_zone
        
        # tstamp_last <- as.POSIXct(time_params$tstamp_last,
        #                           origin = "1970-01-01 00:00:00",
        #                           tz = "UTC")
        # 
        # tstamp_start <- as.POSIXct(time_params$tstamp_start,
        #                 origin = "1970-01-01 00:00:00",
        #                 tz = "UTC")
        # R uses time zone abbreviation to print time stamps,
        # and also get_step_window. On the other hand, pgtraj stores the "long" time zone
        # format (e.g. America/New_York instead of EDT). Thus the warning
        # of In check_tzones(e1, e2) : 'tzone' attributes are inconsistent
        # attributes(time_params$tstamp_start)$tzone <- tzone
        
        increment <- period(num = time_params$increment,
                              units = "seconds")
        
        # default interval is 10*increment (~10 steps)
        limit <- time_params$tstamp_start + (increment * 10)
        if (limit < time_params$tstamp_last) {
            interval <- increment * 10
        } else {
            message("Loading full trajectory, because it is shorter than 10 steps.")
            interval <- time_params$tstamp_last - time_params$tstamp_start
        }
        
        # Get initial set of trajectories
        st <- get_step_window(conn, schema, view, time_params$tstamp_start,
                              interval, FALSE, info_cols,
                              time_params$tstamp_start, time_params$tstamp_last)
        
        # Get full traj
        st_1 <- get_full_traj(conn, schema, view)
        
        # color by animal_name
        # factpal <- colorFactor(topo.colors(4), st$animal_name)
        
        # get animal list
        animals_df <- getAnimalsDf(conn, schema, view)
        colors_animal <- colorFactor(topo.colors(nrow(animals_df)),
                                     animals_df$animal_name,
                                     na.color = "#808080")
        
        # get burst list for burst mode
        bursts_df <- get_bursts_df(conn, schema, view)
        burst_len <- nrow(bursts_df)
        colors_burst <- colorFactor(topo.colors(burst_len),
                                    bursts_df$burst_name,
                                    na.color = "#808080")
        
        unit_init <- "seconds"
        
        # TODO: add validation for burst_len >= 1
        
        # Get background layers
        base <- NULL
        if(!is.null(layers_vector)){
            base <- getLayers(conn, layers_vector)
        }
        if(!is.null(layer_raster)){
            if(class(layer_raster)[1] != "RasterLayer"){
                warning("Please provide a RasterLayer object for layer_raster. Hint: raster::raster()")
                layer_raster <- NULL
            } else {
                raster_name <- deparse(substitute(layer_raster))
            }
        } else {
            raster_name <- NULL
        }
        
        info_cols <- getInfolocsColumns(conn, schema, pgtraj)
            
        ui <-
            fluidPage(# tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
                titlePanel(paste("pgtraj name:", pgtraj)),
                
                sidebarLayout(
                    sidebarPanel(
                        tags$script(
                            '$(document).on("keydown",
                            function (e) {
                            if(e.which == 66) {
                            Shiny.onInputChange("b", new Date() );
                            } else if (e.which == 78) {
                            Shiny.onInputChange("n", new Date() );
                            }
                            });
                            '
                    ),
                    switchInput(
                        inputId = "step_mode",
                        label = "Step mode",
                        value = FALSE
                    ),
                    radioGroupButtons(inputId = "color_choice", 
                                      label = "Color",
                                      choices = c("Animals", "Bursts"),
                                      selected = "Animals"),
                    # h4(strong("Steps")),
                    # h5(textOutput("tstamp")),
                    pickerInput(
                        inputId = "burst_picker",
                        label = "Bursts",
                        choices = bursts_df$burst_name,
                        options = list(`actions-box` = TRUE),
                        multiple = TRUE,
                        width = "100%"
                    ),
                    fluidRow(
                        column(6,
                        numericInput("increment", "Increment",
                                value = increment@.Data,
                                width = "100%"),
                        numericInput("interval", "Interval",
                                value = interval@.Data,
                                width = "100%")
                        ),
                        column(6,
                           selectInput(
                               "increment_unit",
                               label = "units",
                               choices = c(
                                   "years" = "years",
                                   "months" = "months",
                                   "days" = "days",
                                   "hours" = "hours",
                                   "minutes" = "minutes",
                                   "seconds" = "seconds"
                               ),
                               selected = unit_init,
                               width = "100%"
                           ),
                           selectInput(
                               "interval_unit",
                               label = "units",
                               choices = c(
                                   "years" = "years",
                                   "months" = "months",
                                   "days" = "days",
                                   "hours" = "hours",
                                   "minutes" = "minutes",
                                   "seconds" = "seconds"
                               ),
                               selected = unit_init,
                               width = "100%"
                           )
                        )
                    ),
                    sliderInput(
                        "range",
                        "Time window:",
                        min = time_params$tstamp_start,
                        max = time_params$tstamp_last,
                        value = c(time_params$tstamp_start, time_params$tstamp_start + interval),
                        step = increment,
                        timezone = tzone
                    ),
                    actionButton("b", "Back"),
                    actionButton("n", "Next"),
                    h5("press B or N")
                    ),
                    
                    mainPanel(leafletOutput("map"))
            ))
        
        server <- function(input, output, session) {
            w <- reactiveValues(data = st_1)
            x <-
                reactiveValues(
                    currStep = st,
                    counter = 0,
                    burst_counter = 0,
                    burst_name = NULL,
                    bursts = NULL
                )
            
            # get current time window and the next
            timeOut <- reactiveValues(currTime = time_params$tstamp_start,
                                      interval = interval,
                                      increment = increment,
                                      increment_unit = unit_init,
                                      interval_unit = unit_init)
            
            # on step mode switch re-render current traj
            observeEvent(input$step_mode, {
                x$counter <- x$counter + 1
                x$currStep <-
                    get_step_window(conn,
                                    schema,
                                    view,
                                    timeOut$currTime,
                                    timeOut$interval,
                                    input$step_mode,
                                    info_cols,
                                    time_params$tstamp_start,
                                    time_params$tstamp_last)
            })
            
            # convert values in Increment to the selected unit
            observeEvent(input$increment_unit, {
                if(is.null(input$increment) | is.logical(input$increment)){
                    return()
                }
                timeOut$increment <- as.period(timeOut$increment,
                                               unit = input$increment_unit)
                
                updateNumericTimeInput(session, input$increment_unit,
                                       "increment", timeOut$increment)
            })
            
            # convert values in Interval to the selected unit
            observeEvent(input$interval_unit, {
                if(is.null(input$interval) | is.logical(input$interval)){
                    return()
                }
                timeOut$interval <- as.period(timeOut$interval,
                                              unit = input$interval_unit)
                
                updateNumericTimeInput(session, input$interval_unit,
                                       "interval", timeOut$interval)
            })
            
            # set Increment from input field
            observeEvent(input$increment, {
                # do not update Increment in case the input is 0 or empty
                if (is.null(input$increment) |
                    is.logical(input$increment) |
                    identical(input$increment, as.integer(0))) {
                    return()
                }
                timeOut$increment <-
                    setTimeInput(input$increment_unit,
                                 input$increment,
                                 timeOut$increment)
            })
            
            # set Interval from input field
            observeEvent(input$interval, {
                # do not update Interval in case the input is 0 or empty
                if (is.null(input$interval) |
                    is.logical(input$interval) |
                    identical(input$interval, as.integer(0))) {
                    return()
                } else {
                    timeOut$interval <-
                        setTimeInput(input$interval_unit,
                                     input$interval,
                                     timeOut$interval)
                    
                    # update time window slider
                    if (period_to_seconds(timeOut$interval) > period(0)) {
                        updateSliderInput(
                            session,
                            "range",
                            value = c(
                                timeOut$currTime,
                                timeOut$currTime + timeOut$interval
                            ),
                            step = timeOut$increment
                        )
                    }
                }
            })
            
            # set Interval and Time Window from slider
            observeEvent(input$range, {
                # the time window must be "open" in order to increment the time stamp
                # do nothing if the start and end times are equal
                stime <- input$range[1]
                etime <- input$range[2]
                
                print(paste("input$range stime, etime", c(stime, etime)))
                print(paste("stime < etime", stime < etime))
                print(paste("interval", as.period(etime - stime)))
                
                if(stime < etime) {
                    timeOut$currTime <- stime
                    
                    timeOut$interval <- as.period(etime - stime)
                    x$counter <- x$counter + 1
                    x$currStep <-
                        get_step_window(conn,
                                        schema,
                                        view,
                                        timeOut$currTime,
                                        timeOut$interval,
                                        input$step_mode,
                                        info_cols,
                                        time_params$tstamp_start,
                                        time_params$tstamp_last)
                    
                    # update the Interval numeric input
                    # updateNumericTimeInput(session, input$interval_unit,
                    #                        "interval", timeOut$interval)
                } else {
                    print("ignore")
                }
            })
            
            # Only update timestamp on click
            observeEvent(input$n, {
                # the time window must be "open" in order to increment the time stamp
                # do nothing if the start and end times are equal
                stime <- timeOut$currTime + timeOut$increment
                etime <- stime + timeOut$interval
                print(paste("input$n stime, etime", c(stime, etime)))
                print(paste("input$n stime < etime", stime < etime))
                
                if(stime < etime) {
                    # for assigning alternating group names
                    x$counter <- x$counter + 1
                    
                    # update time window slider
                    updateSliderInput(
                        session,
                        "range",
                        value = c(
                            stime,
                            etime
                        ),
                        step = timeOut$increment
                    )
                } else {
                    message("time window out of range")
                }
            })
            
            observeEvent(input$b, {
                stime <- timeOut$currTime - timeOut$increment
                etime <- stime + timeOut$interval
                print(paste("input$b stime, etime", c(stime, etime)))
                print(paste("input$b stime < etime", stime < etime))
                
                if(stime < etime) {
                    # for assigning alternating group names
                    x$counter <- x$counter + 1
                    
                    # update time window slider
                    updateSliderInput(
                        session,
                        "range",
                        value = c(
                            stime,
                            etime
                        ),
                        step = timeOut$increment
                    )
                } else {
                    message("time window out of range")
                }
            })
            
            # # Report current timestamp
            # output$tstamp <- renderText({
            #     paste(
            #         format(timeOut$currTime, usetz = TRUE),
            #         "-",
            #         format(timeOut$currTime + timeOut$interval, usetz = TRUE)
            #     )
            # })
            
            output$map <- renderLeaflet({
                if (is.null(w$data)) {
                    return()
                } else {
                    map <- leaflet() %>%
                        addTiles(group = "OSM (default)")
                    
                    if (!is.null(layer_raster)) {
                        map <- do.call(leaflet::addRasterImage,
                                       c(list(map=map,
                                              x=layer_raster,
                                              group=raster_name),
                                         layers_params_raster)
                                        )
                    }
                    
                    if (!is.null(base)) {
                        for (l in names(base)) {
                            geomtype <- as.character(st_geometry_type(base[[l]])[1])
                            if (geomtype == "raster") {
                                warning("Please provide raster base layers as a layer_raster argument.")
                            } else if (grepl("polygon", geomtype, ignore.case = TRUE)) {
                                map <- do.call(leaflet::addPolygons,
                                               c(list(map=map,
                                                      data=base[[l]],
                                                      group = l),
                                                layers_params_vector[[l]])
                                               )
                            } else if (grepl("linestring", geomtype, ignore.case = TRUE)) {
                                map <- do.call(leaflet::addPolylines,
                                               c(list(map=map,
                                                      data=base[[l]],
                                                      group = l),
                                                 layers_params_vector[[l]])
                                )
                            } else if (grepl("point", geomtype, ignore.case = TRUE)) {
                                map <- do.call(leaflet::addCircleMarkers,
                                               c(list(map=map,
                                                      data=base[[l]],
                                                      group = l),
                                                  layers_params_vector[[l]])
                                               )
                            }
                        }
                    }
                    # prepare layer names for layer control (append() doesn't like NULL values)
                    layer_names <- c("OSM (default)", "trajfull", "bursts")
                    if(!is.null(raster_name)) {layer_names <- 
                        append(layer_names, raster_name)}
                    if(!is.null(base)) {layer_names <- 
                        append(layer_names, names(base))}
                        
                    if (is.null(w$data)) {
                        return()
                    } else {
                        map %>%
                            addPolylines(
                                data = w$data,
                                group = "trajfull",
                                fillOpacity = .5,
                                opacity = .5,
                                color = "orange",
                                weight = 2
                            ) %>% 
                            addLayersControl(
                                overlayGroups = layer_names,
                                options = layersControlOptions(collapsed = FALSE)
                            )
                    }
                }
            })
            
            # add burst to map only when the burst picker is updated, and 
            # only add/remove what is neccessary
            observeEvent(input$burst_picker, {
                burst_get <- setdiff(input$burst_picker, x$burst_name)
                burst_remove <- setdiff(x$burst_name, input$burst_picker)

                # first remove obsolete burst on map
                proxy <- leafletProxy("map") %>%
                    removeShape(burst_remove)
                x$burst_name <- input$burst_picker
                
                # colors
                if(input$color_choice == "Bursts"){
                    colorpal <- ~colors_burst(burst_name)
                } else {
                    colorpal <- ~colors_animal(animal_name)
                }
                
                if (length(burst_get) > 0) {
                    x$bursts <- getBurstGeom(conn, schema, view, burst_get)
                    proxy %>% addPolylines(
                        data = x$bursts,
                        group = "bursts",
                        layerId = burst_get,
                        fillOpacity = 1,
                        opacity = 1,
                        color = colorpal,
                        weight = 4,
                        popup = mapview::popupTable(x$bursts)
                    )
                } 
            })
            
            observe({
                # don't do anything when there is no geometry to display
                if(!is.null(x$currStep)) {
                    # counter for adding/removing groups
                    if (x$counter %% 2 == 0) {
                        gname <- "traj"
                    } else {
                        gname <- "trajnew"
                    }
                    # colors
                    if(input$color_choice == "Bursts"){
                        colorpal <- ~colors_burst(burst_name)
                    } else {
                        colorpal <- ~colors_animal(animal_name)
                    }
                    # map
                    print(x$currStep)
                    proxy <- leafletProxy("map") %>%
                        addPolylines(
                            data = x$currStep,
                            group = gname,
                            fillOpacity = 1,
                            opacity = 1,
                            color = colorpal,
                            weight = 4,
                            popup = mapview::popupTable(x$currStep)
                        )
                    if (x$counter %% 2 == 0) {
                        proxy %>% clearGroup("trajnew")
                    } else {
                        proxy %>% clearGroup("traj")
                    }
                    
                    # because observeEven doesn't pass value when all burst are
                    # deselected
                    if (is.null(input$burst_picker)) {
                        proxy %>% clearGroup("bursts")
                    }
                }
            })
            
        }
        shinyApp(ui, server)
        }

