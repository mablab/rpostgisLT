library(sf)
library(lubridate)
library(shiny)
library(leaflet)
library(dplyr)
library(DBI)
library(htmltools)
library(mapview)
library(shinyWidgets)
library(testthat)


# Queries ------------------------------------------------------------

# Get steps within a temporal window
get_step_window <- function(conn, schema, view, time, interval, step_mode){
    stopifnot(expect_true(is.duration(interval)))
    
    t <- dbQuoteString(conn, format(time, usetz = TRUE))
    t_interval <- dbQuoteString(conn, paste(interval@.Data, "seconds"))
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
                            a.animal_name
                        FROM ", schema_q, ".", view_q, " a
                        WHERE a.relocation_time >= ",t,"::timestamptz
                        AND a.relocation_time < (",t,"::timestamptz + ",
                            t_interval, "::INTERVAL)
                        AND a.step_geom IS NOT NULL
                        GROUP BY a.animal_name;")
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
        
        increment <- duration(num = time_params$increment,
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
        
        # get burst list for burst mode
        bursts_df <- get_bursts_df(conn, schema, view)
        burst_len <- nrow(bursts_df)
        
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
                    h4(strong("Steps")),
                    h5(textOutput("tstamp")),
                    pickerInput(
                        inputId = "burst_picker",
                        label = "Bursts",
                        choices = bursts_df$burst_name,
                        options = list(`actions-box` = TRUE),
                        multiple = TRUE
                    ),
                    numericInput("increment", "Increment:", value = increment@.Data),
                    numericInput("interval", "Interval:", value = interval@.Data),
                    selectInput(
                        "unit",
                        label = NULL,
                        choices = c(
                            "years" = "years",
                            "months" = "months",
                            "weeks" = "weeks",
                            "days" = "days",
                            "hours" = "hours",
                            "minutes" = "minutes",
                            "seconds" = "seconds"
                        ),
                        selected = "seconds"
                    ),
                    actionButton("set_i", "Set"),
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
                                      increment = increment)
            
            observeEvent(input$set_i, {
                if (input$unit == "years") {
                    timeOut$increment <- duration(num = input$increment,
                                                  units = "years")
                    timeOut$interval <- duration(num = input$interval,
                                                 units = "years")
                } else if (input$unit == "months") {
                    timeOut$increment <- duration(num = input$increment,
                                                  units = "months")
                    timeOut$interval <- duration(num = input$interval,
                                                 units = "months")
                } else if (input$unit == "weeks") {
                    timeOut$increment <- duration(num = input$increment,
                                                  units = "weeks")
                    timeOut$interval <- duration(num = input$interval,
                                                 units = "weeks")
                } else if (input$unit == "days") {
                    timeOut$increment <- duration(num = input$increment,
                                                  units = "days")
                    timeOut$interval <- duration(num = input$interval,
                                                 units = "days")
                } else if (input$unit == "hours") {
                    timeOut$increment <- duration(num = input$increment,
                                                  units = "hours")
                    timeOut$interval <- duration(num = input$interval,
                                                 units = "hours")
                } else if (input$unit == "minutes") {
                    timeOut$increment <- duration(num = input$increment,
                                                  units = "minutes")
                    timeOut$interval <- duration(num = input$interval,
                                                 units = "minutes")
                } else if (input$unit == "seconds") {
                    timeOut$increment <- duration(num = input$increment,
                                                  units = "seconds")
                    timeOut$interval <- duration(num = input$interval,
                                                 units = "seconds")
                }
            })
            
            observeEvent(input$range, {
                timeOut$currTime <- input$range[1]
                timeOut$interval <-
                    as.duration(input$range[2] - input$range[1])
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
            
            # Report current timestamp
            output$tstamp <- renderText({
                paste(
                    format(timeOut$currTime, usetz = TRUE),
                    "-",
                    format(timeOut$currTime + timeOut$interval, usetz = TRUE)
                )
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
                
                if (length(burst_get) > 0) {
                    x$bursts <- getBurstGeom(conn, schema, view, burst_get)
                    proxy %>% addPolylines(
                        data = x$bursts,
                        group = "bursts",
                        layerId = burst_get,
                        fillOpacity = 1,
                        opacity = 1,
                        color = "yellow",
                        #~factpal(animal_name),
                        weight = 4,
                        popup = mapview::popupTable(x$bursts)
                    )
                } 
            })
            
            observe({
                if (x$counter %% 2 == 0) {
                    gname <- "traj"
                } else {
                    gname <- "trajnew"
                }
                proxy <- leafletProxy("map") %>%
                    addPolylines(
                        data = x$currStep,
                        group = gname,
                        fillOpacity = 1,
                        opacity = 1,
                        color = "red",
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

