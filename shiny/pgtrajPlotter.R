library(sf)
library(lubridate)
library(shiny)
library(leaflet)
library(dplyr)
library(DBI)
library(htmltools)
library(mapview)
library(shinyWidgets)

# Queries ------------------------------------------------------------

# Get steps within a temporal window
get_step_window <- function(conn, schema, view, time, interval, step_mode){
    stopifnot(is.period(interval))
    i <- period_to_seconds(interval)
    t <- dbQuoteString(conn, format(time, usetz = TRUE))
    t_interval <- dbQuoteString(conn, paste(i, "seconds"))
    schema_q <- dbQuoteIdentifier(conn, schema)
    view_q <- dbQuoteIdentifier(conn, view)
    if(step_mode){
        sql_query <- paste0("
                        SELECT
                            a.step_id,
                            a.step_geom,
                            a.relocation_time,
                            a.burst_name,
                            a.animal_name,
                            a.pgtraj_name
                        FROM ", schema_q, ".", view_q, " a
                        WHERE a.relocation_time >= ",t,"::timestamptz
                        AND a.relocation_time < (",t,"::timestamptz + ",
                            t_interval, "::INTERVAL)
                        AND a.step_geom IS NOT NULL;")
    } else {
        sql_query <- paste0("
                        SELECT
                            st_makeline(a.step_geom)::geometry(
                                linestring,
                                4326
                            ) AS step_geom,
                            a.burst_name,
                            a.animal_name
                        FROM ", schema_q, ".", view_q, " a
                        WHERE a.relocation_time >= ",t,"::timestamptz
                        AND a.relocation_time < (",t,"::timestamptz + ",
                            t_interval, "::INTERVAL)
                        AND a.step_geom IS NOT NULL
                        GROUP BY a.burst_name, a.animal_name;")
    }
    return(st_read_db(conn, query=sql_query, geom_column = "step_geom"))
}

# Get list of bursts in step_geometry view
get_bursts_df <- function(conn, schema, view){
    schema_q <- dbQuoteIdentifier(conn, schema)
    view_q <- dbQuoteIdentifier(conn, view)
    sql_query <- paste0("
                        SELECT
                            DISTINCT burst_name
                        FROM
                            ",schema_q,".", view_q,";")
    return(dbGetQuery(conn, sql_query))
}

# Get list of animals in step_geometry view
getAnimalsDf <- function(conn, schema, view){
    schema_q <- dbQuoteIdentifier(conn, schema)
    view_q <- dbQuoteIdentifier(conn, view)
    sql_query <- paste0("
                        SELECT
                            DISTINCT animal_name
                        FROM
                            ",schema_q,".", view_q,";")
    return(dbGetQuery(conn, sql_query))
}

# Get geometry of bursts as linestring
getBurstGeom <- function(conn, schema, view, burst_name){
    # accepts a character vector of variable length
    
    if (is.null(burst_name) | length(burst_name) == 0){
        return()
    } else if (length(burst_name) == 1) {
        burst_sql <- dbQuoteString(conn, burst_name)
    } else if (length(burst_name) > 1) {
        sql_array <- paste(a$burst_name, collapse = "','")
        burst_sql <- paste0("ANY(ARRAY['",sql_array,"'])")
    }
    
    schema_q <- dbQuoteIdentifier(conn, schema)
    view_q <- dbQuoteIdentifier(conn, view)
    sql_query <- paste0("
                        SELECT
                            st_makeline(step_geom)::geometry(
                                linestring,
                                4326
                            ) AS burst_geom,
                            burst_name,
                            animal_name
                        FROM
                            ",schema_q,".", view_q,"
                        WHERE
                            burst_name = ", burst_sql, "
                        GROUP BY
                            burst_name,
                            animal_name;")
    return(st_read_db(conn, query=sql_query, geom_column = "burst_geom"))
}

# Get the complete trajectory of an animal as a single linestring
get_full_traj <- function(conn, schema, view){
    sql_query <- paste0("
                        SELECT
                        st_makeline(step_geom)::geometry(linestring, 4326) AS traj_geom,
                        animal_name
                        FROM ", schema, ".", view, "
                        GROUP BY animal_name;")
    return(st_read_db(conn, query=sql_query, geom_column = "traj_geom"))
}

# Get default time parameters
get_traj_defaults <- function(conn, schema, view, pgtraj){
    schema_q <- dbQuoteIdentifier(conn, schema)
    view_q <- dbQuoteIdentifier(conn, view)
    sql_query <- paste0("
                        SELECT time_zone
                        FROM ", schema, ".pgtraj
                        WHERE pgtraj_name = ", dbQuoteString(conn, pgtraj),
                        ";")
    tzone <- dbGetQuery(conn, sql_query)
    
    # default increment is the median step duration
    sql_query <- paste0("
                        SELECT
                            EXTRACT(
                                epoch
                            FROM
                                MIN( relocation_time )
                            ) AS tstamp_start,
                            EXTRACT(
                                epoch
                            FROM
                                MAX( relocation_time )
                            ) AS tstamp_last,
                            EXTRACT(
                                epoch
                            FROM
                                PERCENTILE_CONT( 0.5 ) WITHIN GROUP(
                                ORDER BY
                                    dt
                                )
                            ) AS increment
                        FROM ",schema_q,".", view_q,";")
    
    time_params <- dbGetQuery(conn, sql_query)
    
    return(cbind(time_params, tzone))
}

updateNumericTimeInput <- function(session, inputUnit, input_type, reactiveTime){
    if (inputUnit == "years") {
        updateNumericInput(session, input_type,
                           value = reactiveTime@year)
    } else if (inputUnit == "months") {
        updateNumericInput(session, input_type,
                           value = reactiveTime@month)
    } else if (inputUnit == "days") {
        updateNumericInput(session, input_type,
                           value = reactiveTime@day)
    } else if (inputUnit == "hours") {
        updateNumericInput(session, input_type,
                           value = reactiveTime@hour)
    } else if (inputUnit == "minutes") {
        updateNumericInput(session, input_type,
                           value = reactiveTime@minute)
    } else if (inputUnit == "seconds") {
        updateNumericInput(session, input_type,
                           value = reactiveTime@.Data)
    }
}

setTimeInput <- function(inputUnit, inputTime, reactiveTime){
    if (inputUnit == "years") {
        reactiveTime <- period(num = inputTime,
                                    units = "years")
    } else if (inputUnit == "months") {
        reactiveTime <- period(num = inputTime,
                                    units = "months")
    } else if (inputUnit == "days") {
        reactiveTime <- period(num = inputTime,
                                    units = "days")
    } else if (inputUnit == "hours") {
        reactiveTime <- period(num = inputTime,
                                    units = "hours")
    } else if (inputUnit == "minutes") {
        reactiveTime <- period(num = inputTime,
                                    units = "minutes")
    } else if (inputUnit == "seconds") {
        reactiveTime <- period(num = inputTime,
                                    units = "seconds")
    }
    
    return(reactiveTime)
}

# Shiny App----------------------------------------------------------------


pgtrajPlotter <-
    function(conn,
             schema,
             pgtraj) {
        view <- paste0("step_geometry_shiny_", pgtraj)
        # Get default time parameters
        time_params <- get_traj_defaults(conn, schema, view, pgtraj)
        
        tzone <- time_params$time_zone
        
        tstamp_last <- as.POSIXct(time_params$tstamp_last,
                                  origin = "1970-01-01 00:00:00",
                                  tz = "UTC")
        attributes(t)$tzone <- tzone
        
        t <- as.POSIXct(time_params$tstamp_start,
                        origin = "1970-01-01 00:00:00",
                        tz = "UTC")
        # R uses time zone abbreviation to print time stamps,
        # thus get_step_window while pgtraj stores the "long" time zone
        # format (e.g. America/New_York instead of EDT). Thus the warning
        # of In check_tzones(e1, e2) : 'tzone' attributes are inconsistent
        attributes(t)$tzone <- tzone
        
        increment <- period(num = time_params$increment,
                              units = "seconds")
        
        # default interval is 10*increment (~10 steps)
        limit <- t + (increment * 10)
        if (limit < tstamp_last) {
            interval <- increment * 10
        } else {
            message("Loading full trajectory, because it is shorter than 10 steps.")
            interval <- tstamp_last - t
        }
        
        # Get initial set of trajectories
        st <-
            get_step_window(conn, schema, view, t, interval, FALSE)
        
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
                        min = t,
                        max = tstamp_last,
                        value = c(t, t + interval),
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
            timeOut <- reactiveValues(currTime = t,
                                      interval = interval,
                                      increment = increment,
                                      increment_unit = unit_init,
                                      interval_unit = unit_init)
            
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
                if(is.null(input$increment) | is.logical(input$increment)){
                    return()
                }
                timeOut$increment <- setTimeInput(input$increment_unit,
                                                 input$increment,
                                                    timeOut$increment)
            })
            
            # set Interval from input field
            observeEvent(input$interval, {
                if(is.null(input$interval) | is.logical(input$interval)){
                    return()
                }
                timeOut$interval <- setTimeInput(input$interval_unit, input$interval,
                             timeOut$interval)
            })
            
            # set Interval and Time Window from slider
            observeEvent(input$range, {
                timeOut$currTime <- input$range[1]
                
                timeOut$interval <-
                    as.period(input$range[2] - input$range[1])
                x$counter <- x$counter + 1
                x$currStep <-
                    get_step_window(conn,
                                    schema,
                                    view,
                                    timeOut$currTime,
                                    timeOut$interval,
                                    input$step_mode)
            })
            
            # Only update timestamp on click
            observeEvent(input$n, {
                # for assigning alternating group names
                x$counter <- x$counter + 1
                
                timeOut$currTime <- timeOut$currTime + timeOut$increment
                
                x$currStep <-
                    get_step_window(
                        conn,
                        schema,
                        view,
                        timeOut$currTime,
                        timeOut$interval,
                        input$step_mode
                    )
            })
            
            observeEvent(input$b, {
                # for assigning alternating group names
                x$counter <- x$counter + 1

                timeOut$currTime <- timeOut$currTime - timeOut$increment
                
                x$currStep <-
                    get_step_window(
                        conn,
                        schema,
                        view,
                        timeOut$currTime,
                        timeOut$interval,
                        input$step_mode
                    )
            })
            
            # # Report current timestamp
            # output$tstamp <- renderText({
            #     paste(
            #         format(timeOut$currTime, usetz = TRUE),
            #         "-",
            #         format(timeOut$currTime + timeOut$interval, usetz = TRUE)
            #     )
            # })
            
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
                            color = "blue",
                            #~factpal(animal_name),
                            weight = 2
                        ) %>%
                        addLayersControl(
                            overlayGroups = c("OSM (default)", "trajfull",
                                              "bursts"),
                            options = layersControlOptions(collapsed = FALSE)
                        )
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
                proxy <- leafletProxy("map") %>%
                    addPolylines(
                        data = x$currStep,
                        group = gname,
                        fillOpacity = 1,
                        opacity = 1,
                        color = colorpal,
                        #~factpal(animal_name),
                        weight = 4,
                        popup = mapview::popupTable(x$currStep)
                    )
                if (x$counter %% 2 == 0) {
                    proxy %>% clearGroup("trajnew")
                } else {
                    proxy %>% clearGroup("traj")
                }
                
                # update time window slider
                updateSliderInput(
                    session,
                    "range",
                    value = c(
                        timeOut$currTime,
                        timeOut$currTime + timeOut$interval
                    ),
                    step = timeOut$increment
                )
                
                # because observeEven doesn't pass value when all burst are
                # deselected
                if (is.null(input$burst_picker)) {
                    proxy %>% clearGroup("bursts")
                }
            })
            
        }
        shinyApp(ui, server)
        }

