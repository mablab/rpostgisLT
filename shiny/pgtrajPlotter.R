library(sf)
library(lubridate)
library(shiny)
library(leaflet)
library(dplyr)
library(RPostgreSQL)


# Queries ------------------------------------------------------------

# Get steps within a temporal window
get_t_window <- function(conn, schema, view, time, interval){
    t <- dbQuoteString(conn, format(time, usetz = TRUE))
    t_interval <- dbQuoteString(conn, paste(interval, "hour"))
    schema_q <- dbQuoteIdentifier(conn, schema)
    view_q <- dbQuoteIdentifier(conn, view)
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

burst_list <- get_burst_list(conn, schema, "step_geometry_shiny_ibex")
burst_counter <- 1
burst_len <- nrow(burst_list)
paste0(burst_counter, "/", burst_len)
burst_name <- burst_list[burst_counter, "burst_name"]
print(burst_name)

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

get_burst_geom(conn, schema, "step_geometry_shiny_ibex", "A153")

# Get the complete trajectory of an animal as a single linestring
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


pgtrajPlotter <-
    function(conn,
             schema,
             pgtraj,
             d_start,
             t_start,
             tzone,
             increment,
             nr_increment,
             interval) {
        view <- paste0("step_geometry_shiny_", pgtraj)
        # Start time
        t <- ymd_hms(paste(d_start, t_start), tz = tzone)
        # Get initial set of trajectories
        st <- get_t_window(conn, schema, view, t, interval)
        
        # Get full traj
        st.1 <- get_full_traj(conn, schema, view)
        
        factpal <- colorFactor(topo.colors(4), st$animal_name)
        
    ui <- fluidPage(
        # tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
        sidebarLayout(
            sidebarPanel(
                h4(strong("Time window")),
                h5(textOutput("tstamp")),
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
                actionButton("b", "Back"),
                actionButton("n", "Next"),
                h5("press B or N"),
                radioButtons("step_burst", label = h4(strong("Increment")),
                             choices = list("Bursts" = "burst",
                                            "Steps" = "step"),
                             selected = "burst")
            ),
            mainPanel(
                leafletOutput("map")
            )
        )
    )
    
    server <- function(input, output) {
        
        w <- reactiveValues(data = st.1)
        x <- reactiveValues(currStep = st, counter = 0)
        # get current time window and the next
        timeOut <- reactiveValues(currTime = t)
        
        # Only update timestamp on click
        observeEvent(input$n, {
            # for assigning alternating group names
            x$counter <- x$counter + 1
            timeOut$currTime <- timeOut$currTime + duration(hour = increment)
            x$currStep <- get_t_window(conn, schema, view, timeOut$currTime, interval)
        })
        
        observeEvent(input$b, {
            # for assigning alternating group names
            x$counter <- x$counter + 1
            timeOut$currTime <- timeOut$currTime - duration(hour = increment)
            x$currStep <- get_t_window(conn, schema, view, timeOut$currTime, interval)
        })
        
        # Report current timestamp
        output$tstamp <- renderText({
            paste(format(timeOut$currTime, usetz = TRUE),
                  "—",
                  format(timeOut$currTime + duration(hour = interval), usetz = TRUE))
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
                    color = ~factpal(animal_name),
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

