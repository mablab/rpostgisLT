server <- function(input, output, session) {
    w <- shiny::reactiveValues(data = st_1)
    x <-
        shiny::reactiveValues(
            currStep = NULL,
            counter = 0,
            burst_counter = 0,
            burst_name = NULL,
            bursts = NULL
        )
    
    # get current time window and the next
    timeOut <- shiny::reactiveValues(currTime = time_params$tstamp_start,
                                     interval = interval,
                                     increment = increment,
                                     increment_unit = unit_init,
                                     interval_unit = unit_init)
    
    # on step mode switch re-render current traj
    shiny::observeEvent(input$step_mode, {
        x$counter <- x$counter + 1
        x$currStep <-
            getStepWindow(
                conn,
                schema,
                view,
                timeOut$currTime,
                timeOut$interval,
                input$step_mode,
                info_cols,
                time_params$tstamp_start,
                time_params$tstamp_last
            )
    },
    ignoreInit = TRUE)
    
    # Interval/Increment input -----------------------------------------
    
    # convert values in Increment to the selected unit
    shiny::observeEvent(input$increment_unit, {
        if (is.null(input$increment) | is.logical(input$increment)) {
            return()
        }
        timeOut$increment <-
            lubridate::as.period(timeOut$increment,
                                 unit = input$increment_unit)
        
        updateNumericTimeInput(session,
                               input$increment_unit,
                               "increment",
                               timeOut$increment)
    })
    
    # convert values in Interval to the selected unit
    shiny::observeEvent(input$interval_unit, {
        if (is.null(input$interval) | is.logical(input$interval)) {
            return()
        }
        timeOut$interval <-
            lubridate::as.period(timeOut$interval,
                                 unit = input$interval_unit)
        
        updateNumericTimeInput(session,
                               input$interval_unit,
                               "interval",
                               timeOut$interval)
    })
    
    # set Increment from input field
    shiny::observeEvent(input$increment, {
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
    shiny::observeEvent(input$interval, {
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
            if (lubridate::period_to_seconds(timeOut$interval) > lubridate::period(0)) {
                shiny::updateSliderInput(
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
    
    # Time slider ------------------------------------------------------
    
    # set Interval and Time Window from slider
    shiny::observeEvent(input$range, {
        # the time window must be "open" in order to increment the time stamp
        # do nothing if the start and end times are equal
        stime <- input$range[1]
        etime <- input$range[2]
        
        if (stime < etime) {
            timeOut$currTime <- stime
            
            timeOut$interval <-
                lubridate::as.period(etime - stime)
            # for assigning alternating group names in order to
            # remove the previous step from the plot
            x$counter <- x$counter + 1
            
            x$currStep <-
                getStepWindow(
                    conn,
                    schema,
                    view,
                    timeOut$currTime,
                    timeOut$interval,
                    input$step_mode,
                    info_cols,
                    time_params$tstamp_start,
                    time_params$tstamp_last
                )
        } else {
            # ignore input
        }
    })
    
    # Keyboard contol --------------------------------------------------
    
    # Only update timestamp on click
    shiny::observeEvent(input$n, {
        # the time window must be "open" in order to increment the time stamp
        # do nothing if the start and end times are equal
        stime <- timeOut$currTime + timeOut$increment
        etime <- stime + timeOut$interval
        
        if (stime < etime) {
            # update time window slider
            shiny::updateSliderInput(
                session,
                "range",
                value = c(stime,
                          etime),
                step = timeOut$increment
            )
        } else {
            message("time window out of range")
        }
    })
    
    shiny::observeEvent(input$b, {
        stime <- timeOut$currTime - timeOut$increment
        etime <- stime + timeOut$interval
        
        if (stime < etime) {
            # update time window slider
            shiny::updateSliderInput(
                session,
                "range",
                value = c(stime,
                          etime),
                step = timeOut$increment
            )
        } else {
            message("time window out of range")
        }
    })
    
    # Leaflet start ----------------------------------------------------
    
    output$map <- leaflet::renderLeaflet({
        if (is.null(w$data)) {
            return()
        } else {
            map <- leaflet::leaflet() %>%
                leaflet::addTiles(group = "OSM (default)")
            
            # Add base layers --------------------------------------------------
            
            # raster layers
            if (!is.null(layer_raster)) {
                map <- do.call(leaflet::addRasterImage,
                               c(
                                   list(map = map,
                                        x = layer_raster,
                                        group = raster_name),
                                   layers_params_raster
                               ))
            }
            
            # vector layers
            if (!is.null(base)) {
                for (l in names(base)) {
                    geomtype <- as.character(sf::st_geometry_type(base[[l]])[1])
                    if (geomtype == "raster") {
                        warning("Please provide raster base layers as a layer_raster argument.")
                    } else if (grepl("polygon", geomtype, ignore.case = TRUE)) {
                        map <- do.call(leaflet::addPolygons,
                                       c(
                                           list(
                                               map = map,
                                               data = base[[l]],
                                               group = l
                                           ),
                                           layers_params_vector[[l]]
                                       ))
                    } else if (grepl("linestring", geomtype, ignore.case = TRUE)) {
                        map <- do.call(leaflet::addPolylines,
                                       c(
                                           list(
                                               map = map,
                                               data = base[[l]],
                                               group = l
                                           ),
                                           layers_params_vector[[l]]
                                       ))
                    } else if (grepl("point", geomtype, ignore.case = TRUE)) {
                        map <- do.call(leaflet::addCircleMarkers,
                                       c(
                                           list(
                                               map = map,
                                               data = base[[l]],
                                               group = l
                                           ),
                                           layers_params_vector[[l]]
                                       ))
                    }
                }
            }
            # prepare layer names for layer control (append() doesn't like NULL values)
            layer_names <-
                c("OSM (default)", "trajfull", "bursts")
            if (!is.null(raster_name)) {
                layer_names <-
                    append(layer_names, raster_name)
            }
            if (!is.null(base)) {
                layer_names <-
                    append(layer_names, names(base))
            }
            
            # Add full traj and legend -------------------------------
            
            if (is.null(w$data)) {
                return()
            } else {
                map %>%
                    leaflet::addPolylines(
                        data = w$data,
                        group = "trajfull",
                        fillOpacity = .5,
                        opacity = .5,
                        color = "orange",
                        weight = 2
                    ) %>%
                    leaflet::addLayersControl(
                        overlayGroups = layer_names,
                        options = leaflet::layersControlOptions(collapsed = FALSE)
                    )
            }
        }
    })
    
    # Add bursts -------------------------------------------------------
    
    # add burst to map only when the burst picker is updated, and
    # only add/remove what is neccessary
    shiny::observeEvent(input$burst_picker, {
        burst_get <- setdiff(input$burst_picker, x$burst_name)
        
        burst_remove <-
            setdiff(x$burst_name, input$burst_picker)
        
        # first remove obsolete burst on map
        proxy <- leaflet::leafletProxy("map") %>%
            leaflet::removeShape(burst_remove)
        x$burst_name <- input$burst_picker
        
        # colors
        if (input$color_choice == "Bursts") {
            colorpal <- ~ colors_burst(burst_name)
        } else {
            colorpal <- ~ colors_animal(animal_name)
        }
        
        if (length(burst_get) > 0) {
            x$bursts <- getBurstGeom(conn, schema, view, burst_get)
            proxy %>% leaflet::addPolylines(
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
    
    # leafletProxy -----------------------------------------------------
    
    shiny::observe({
        # don't do anything when there is no geometry to display
        if (!is.null(x$currStep)) {
            # counter for adding/removing the next/previous set of steps
            # when plotting a trajectory
            if (x$counter %% 2 == 0) {
                gname <- "traj"
            } else {
                gname <- "trajnew"
            }
            # colors
            if (input$color_choice == "Bursts") {
                colorpal <- ~ colors_burst(burst_name)
            } else {
                colorpal <- ~ colors_animal(animal_name)
            }
            # map
            if (length(sf::st_geometry(x$currStep)) > 0) {
                proxy <- leaflet::leafletProxy("map") %>%
                    leaflet::addPolylines(
                        data = x$currStep,
                        group = gname,
                        fillOpacity = 1,
                        opacity = 1,
                        color = colorpal,
                        weight = 4,
                        popup = mapview::popupTable(x$currStep)
                    )
                
                if (x$counter %% 2 == 0) {
                    proxy %>% leaflet::clearGroup("trajnew")
                } else {
                    proxy %>% leaflet::clearGroup("traj")
                }
                
                # because observeEven doesn't pass value when all burst are
                # deselected
                if (is.null(input$burst_picker)) {
                    proxy %>% leaflet::clearGroup("bursts")
                }
                
            } else {
                # leaflet crashes
            }
        }
    })
    
}