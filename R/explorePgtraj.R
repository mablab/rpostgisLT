#' Explore a pgtraj interactively in a Shiny app
#'
#' @param conn DBI::DBIConnection
#' @param schema String. Schema name of the pgtraj.
#' @param pgtraj String. Pgtraj name.
#' @param layers_vector List of character vectors. As c(schema, table).
#' @param layers_params_vector Named list of lists. Names need to map to the
#'  table names in layers_vector. Sub-lists contain parameters passed to
#'  leaflet::add*. See example.
#' @param layer_raster raster::RasterLayer object
#' @param layers_params_raster List. Parameters passed to leaflet::addRasterImage()
#'
#' @return nothing
#' @export
#' @importFrom magrittr "%>%"
#' 
#' @author Balázs Dukai \email{balazs.dukai@@gmail.com}
#'
#' @examples
#' \dontrun{
#' # Vectore base layers to include
#' layers_vector <- list(c("example_data", "county_subdiv"),
#'                       c("example_data", "test_points")
#'                       )
#' layers_params_vector <- list(test_points=list(color = "red",
#'                                               stroke = FALSE,
#'                                               fillOpacity = 0.5),
#'                              county_subdiv=list(color = "grey",
#'                                                 fillOpacity = 0.2)
#'                              )
#' 
#' # Raster base layers to include
#' ras <- rgdal::readGDAL("./temp_data/florida_dem_county099.tif")
#' ras2 <- raster::raster(ras, 1)
#' ras2_leaflet <- leaflet::projectRasterForLeaflet(ras2)
#' explorePgtraj(conn, schema, pgtraj, layers_vector, layers_params_vector,
#'               layer_raster=ras2_leaflet)
#' }
explorePgtraj <-
    function(conn,
             schema,
             pgtraj,
             layers_vector=NULL,
             layers_params_vector=NULL,
             layer_raster=NULL,
             layers_params_raster=NULL) {
        view <- paste0("step_geometry_shiny_", pgtraj)
        # Get default time parameters
        time_params <- getTrajDefaults(conn, schema, view, pgtraj)

        tzone <- time_params$time_zone

        increment <- lubridate::period(num = time_params$increment,
                                       units = "seconds")

        # default interval is 10*increment (~10 steps)
        limit <- time_params$tstamp_start + (increment * 10)
        if (limit < time_params$tstamp_last) {
            interval <- increment * 10
        } else {
            message("Loading full trajectory, because it is shorter than 10 steps.")
            interval <-
                time_params$tstamp_last - time_params$tstamp_start
        }

        # Get full traj
        st_1 <- getFullTraj(conn, schema, view)

        # get animal list
        animals_df <- getAnimalsDf(conn, schema, view)
        colors_animal <-
            leaflet::colorFactor(grDevices::topo.colors(nrow(animals_df)),
                                 animals_df$animal_name,
                                 na.color = "#808080")

        # get burst list for burst mode
        bursts_df <- getBurstsDF(conn, schema, view)
        burst_len <- nrow(bursts_df)
        colors_burst <-
            leaflet::colorFactor(grDevices::topo.colors(burst_len),
                                 bursts_df$burst_name,
                                 na.color = "#808080")

        # initial unit for interval/increment
        unit_init <- "seconds"

        # TODO: add validation for burst_len >= 1

        # Get background layers
        base <- NULL
        if (!is.null(layers_vector)) {
            base <- getLayers(conn, layers_vector)
        }
        if (!is.null(layer_raster)) {
            if (class(layer_raster)[1] != "RasterLayer") {
                warning("Please provide a RasterLayer object for layer_raster. Hint: raster::raster()")
                layer_raster <- NULL
            } else {
                raster_name <- deparse(substitute(layer_raster))
            }
        } else {
            raster_name <- NULL
        }

        info_cols <- getInfolocsColumns(conn, schema, pgtraj)
        
        # UI start -------------------------------------------------------------
        
        ui <-
            shiny::navbarPage("explorePgtraj", id="nav",
                shiny::tabPanel(
                    "Interactive map",

                    shiny::div(
                        class = "outer",

                        shiny::tags$head(
                            shiny::includeCSS(system.file("shinyapp","styles.css",
                                                          package = "rpostgisLT")),
                            shiny::includeScript(system.file("shinyapp","keypress.js",
                                                             package = "rpostgisLT"))
                        ),

                        leaflet::leafletOutput("map", width = "100%", height = "100%"),

                        shiny::absolutePanel(
                            id = "controls",
                            class = "panel panel-default",
                            fixed = TRUE,
                            draggable = TRUE,
                            top = 60,
                            left = "auto",
                            right = 20,
                            bottom = "auto",
                            width = 330,
                            height = "auto",

                            shiny::h2(paste("pgtraj:", pgtraj)),

                            shinyWidgets::switchInput(
                                inputId = "step_mode",
                                label = "Step mode",
                                value = FALSE
                            ),
                            shinyWidgets::radioGroupButtons(
                                inputId = "color_choice",
                                label = "Color",
                                choices = c("Animals", "Bursts"),
                                selected = "Animals"
                            ),

                            shiny::selectizeInput(
                                inputId = "burst_picker",
                                label = "Bursts",
                                choices = bursts_df$burst_name,
                                multiple = TRUE
                            ),

                            shiny::fluidRow(
                                shiny::column(
                                    6,
                                    shiny::numericInput(
                                        "increment",
                                        "Increment",
                                        value = increment@.Data,
                                        width = "100%"
                                    ),
                                    shiny::numericInput(
                                        "interval",
                                        "Interval",
                                        value = interval@.Data,
                                        width = "100%"
                                    )
                                ),
                                shiny::column(
                                    6,
                                    shiny::selectInput(
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
                                    shiny::selectInput(
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

                            shiny::sliderInput(
                                "range",
                                "Time window:",
                                min = time_params$tstamp_start,
                                max = time_params$tstamp_last,
                                value = c(time_params$tstamp_start,
                                          time_params$tstamp_start + interval),
                                step = increment,
                                timezone = tzone
                            ),

                            shiny::tags$div(
                                style="display:inline-block",
                                title = "Press <-",
                                shiny::actionButton("b", "Back")
                            ),

                            shiny::tags$div(
                                style="display:inline-block",
                                title = "Press ->",
                                shiny::actionButton("n", "Next")
                            )
                        )
                    )
                ),
                shiny::tabPanel("Data explorer",
                                shiny::h5("This is not implemented yet :( Wanna help? Visit:"),
                                shiny::h5(shiny::a("https://github.com/mablab/rpostgisLT"))
                )
            )

        
        # Server start ---------------------------------------------------------
        
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
                        c("OSM (default)", "Full trajectory", "Bursts")
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
                                group = "Full trajectory",
                                fillOpacity = .5,
                                opacity = .5,
                                color = "orange",
                                weight = 2
                            ) %>%
                            leaflet::addLayersControl(
                                overlayGroups = layer_names,
                                options = leaflet::layersControlOptions(collapsed = FALSE),
                                position = "topleft"
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
                        group = "Bursts",
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
                            proxy %>% leaflet::clearGroup("Bursts")
                        }

                    } else {
                        # leaflet crashes
                    }
                }
            })

        }
        
        shiny::shinyApp(ui, server)
}


