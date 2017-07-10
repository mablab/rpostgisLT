library(sf)
library(lubridate)
library(shiny)
library(leaflet)
library(dplyr)
library(DBI)
library(htmltools)
library(mapview)
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
get_burst_list <- function(conn, schema, view){
    schema_q <- dbQuoteIdentifier(conn, schema)
    view_q <- dbQuoteIdentifier(conn, view)
    sql_query <- paste0("
                        SELECT
                            DISTINCT burst_name
                        FROM
                            ",schema_q,".", view_q,";")
    return(dbGetQuery(conn, sql_query))
}

# Get geometry of a single burst linestring
get_burst_geom <- function(conn, schema, view, burst_name){
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
                            burst_name = ", dbQuoteString(conn, burst_name),"
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
             pgtraj,
             d_start=NULL,
             t_start=NULL,
             increment=NULL,
             interval=NULL) {
        view <- paste0("step_geometry_shiny_", pgtraj)
        # Get default time parameters
        time_params <- get_traj_defaults(conn, schema, view, pgtraj)
        
        tzone <- time_params$time_zone
        
        tstamp_last <- as.POSIXct(time_params$tstamp_last,
                                 origin = "1970-01-01 00:00:00",
                                 tz = "UTC")
        attributes(t)$tzone <- tzone
        
        if(is.null(d_start)){
            expect_null(t_start)
            t <- as.POSIXct(time_params$tstamp_start,
                            origin = "1970-01-01 00:00:00",
                            tz = "UTC")
            # R uses time zone abbreviation to print time stamps,
            # thus get_step_window while pgtraj stores the "long" time zone
            # format (e.g. America/New_York instead of EDT). Thus the warning
            # of In check_tzones(e1, e2) : 'tzone' attributes are inconsistent
            attributes(t)$tzone <- tzone
        } else {
            t <- ymd_hms(paste(d_start, t_start), tz = tzone)
        }
        
        if(is.null(increment)){
            increment <- duration(num = time_params$increment,
                                  units = "seconds")
        } else {
            increment <- duration(num = increment, units = "seconds")
        }
        
        # default interval is 10*increment (~10 steps)
        if(is.null(interval)){
            limit <- t + (increment * 10)
            if(limit < tstamp_last) {
                interval <- increment * 10
            } else {
                message("Loading full trajectory, because it is shorter than 10 steps.")
                interval <- tstamp_last - t
            }
        } else {
            interval <- duration(num = interval, units = "seconds")
        }
        
        # Get initial set of trajectories
        st <- get_step_window(conn, schema, view, t, interval, FALSE)
        
        # Get full traj
        st.1 <- get_full_traj(conn, schema, view)
        
        # color by animal_name
        # factpal <- colorFactor(topo.colors(4), st$animal_name)
        
        # get burst list for burst mode
        burst_list <- get_burst_list(conn, schema, view)
        burst_len <- nrow(burst_list)
        
        # TODO: add validation for burst_len >= 1
        
    ui <- fluidPage(
        # tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
        titlePanel(paste("pgtraj name:", pgtraj)),
        
        sidebarLayout(
            
            sidebarPanel(
                tags$script('$(document).on("keydown",
                    function (e) {
                        var d = new Date(); 
                        if(e.which == 66) {
                            Shiny.onInputChange("b",
                                Math.round((d.getMilliseconds()+251) / 500) );
                        } else if (e.which == 78) {
                            Shiny.onInputChange("n",
                                Math.round((d.getMilliseconds()+251) / 500) );
                        }
                    });
                    '),
                radioButtons("step_burst", label = h4(strong("Increment")),
                             choices = list("Bursts" = "burst",
                                            "Steps" = "step"),
                             selected = "step"),
                checkboxInput("step_mode", label = "Step mode", value = FALSE),
                h4(strong("Steps")),
                h5(textOutput("tstamp")),
                h4(strong("Burst")),
                h5(textOutput("burst_name")),
                h5(textOutput("burst_counter")),
                # numericInput("interval", "Interval:", value = interval@.Data),
                # selectInput("interval_unit", label = NULL, 
                #             choices = c("month" = "month",
                #                                "week" = "week",
                #                                "day" = "day",
                #                                "hour" = "hour",
                #                                "minute" = "minute",
                #                                "second" = "second"),
                #             selected = "second"),
                sliderInput("range", "Time window:", min = t, max = tstamp_last,
                            value = c(t, t + interval), step = increment,
                            timezone = tzone),
                # actionButton("submit_range", "Set range"),
                h5(textOutput("increment")),
                actionButton("b", "Back"),
                actionButton("n", "Next"),
                h5("press B or N")
            ),
            
            mainPanel(
                leafletOutput("map")
            )
        )
    )
    
    server <- function(input, output, session) {
        
        w <- reactiveValues(data = st.1)
        x <- reactiveValues(currStep = st, counter = 0, burst_counter = 0,
                            burst_name = "")
        # get current time window and the next
        timeOut <- reactiveValues(currTime = t, interval = interval)
        
        observeEvent(input$range, {
            timeOut$currTime <- input$range[1]
            timeOut$interval <- as.duration(input$range[2] - input$range[1])
            x$counter <- x$counter + 1
            x$currStep <- get_step_window(conn, schema, view, timeOut$currTime,
                                          timeOut$interval, input$step_mode)
        })
        
        # Only update timestamp on click
        observeEvent(input$n, {
            # for assigning alternating group names
            x$counter <- x$counter + 1
            
            if(input$step_burst == "burst") {
                if(x$burst_counter < burst_len) {
                    x$burst_counter <- x$burst_counter + 1
                    x$burst_name <- burst_list[x$burst_counter, "burst_name"]
                    x$currStep <- get_burst_geom(conn, schema, view, x$burst_name)
                }
            } else if (input$step_burst == "step") {
                timeOut$currTime <- timeOut$currTime + increment
                x$currStep <- get_step_window(conn, schema, view, timeOut$currTime,
                                           timeOut$interval, input$step_mode)
            }
        })
        
        observeEvent(input$b, {
            # for assigning alternating group names
            x$counter <- x$counter + 1
            
            if(input$step_burst == "burst") {

                if(x$burst_counter > 1){
                    x$burst_counter <- x$burst_counter - 1
                    x$burst_name <- burst_list[x$burst_counter, "burst_name"]
                    x$currStep <- get_burst_geom(conn, schema, view, x$burst_name)
                }
            } else if (input$step_burst == "step") {
                timeOut$currTime <- timeOut$currTime - increment
                x$currStep <- get_step_window(conn, schema, view, timeOut$currTime,
                                              timeOut$interval, input$step_mode)
            }

        })
        
        # Report current timestamp
        output$tstamp <- renderText({
            paste(format(timeOut$currTime, usetz = TRUE),
                  "-",
                  format(timeOut$currTime + timeOut$interval, usetz = TRUE))
        })
        
        output$increment <- renderText({
            paste("Increment is", increment)
        })
        
        # Report current burst name and count
        output$burst_name <- renderText({
            x$burst_name
        })
        
        output$burst_counter <- renderText({
            paste0(x$burst_counter, "/", burst_len)
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
                        color = "blue", #~factpal(animal_name),
                        weight = 2
                    ) %>%
                    addLayersControl(
                        overlayGroups = c("OSM (default)", "trajfull"),
                        options = layersControlOptions(collapsed = FALSE)
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
                    color = "red", #~factpal(animal_name),
                    weight = 4,
                    popup = mapview::popupTable(x$currStep)
                )
            if (x$counter %% 2 == 0) {
                proxy %>% clearGroup("trajnew")
            } else {
                proxy %>% clearGroup("traj")
            }
            
            # update time window slider
            updateSliderInput(session, "range",
                              value = c(timeOut$currTime,
                                        timeOut$currTime + timeOut$interval))
        })
        
    }
    shinyApp(ui, server)
}

