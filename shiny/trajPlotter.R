library(sf)
library(lubridate)
library(shiny)
library(leaflet)
library(dplyr)


# Queries ------------------------------------------------------------

get_t_window <- function(conn, schema, view, time, interval){
    # t_start <- paste(start_date, start_hour)
    t <- format(time, usetz = TRUE)
    t_interval <- paste(interval, "hour")
    sql_query <- paste0("
                        SELECT
                            a.step_id,
                            a.step_geom,
                            a.relocation_time,
                            a.burst_name,
                            a.animal_name,
                            a.pgtraj_name
                        FROM ", schema, ".", view, " a
                        WHERE a.relocation_time >= '", t, "'::timestamptz
                        AND a.relocation_time < ('", t, "'::timestamptz + '",
                            t_interval, "'::INTERVAL)
                        AND a.step_geom IS NOT NULL;")
    # s <- gsub("\n", "", sql_query)
    # print(s)
    return(st_read_db(conn, query=sql_query, geom_column = "step_geom"))
    # return(pgGetGeom(conn, query = sql_query))
}


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

pgtrajPlotter <- function(conn, schema, pgtraj, d_start, t_start, tzone, increment,
                           nr_increment, interval) {
    view <- paste0("step_geometry_shiny_", pgtraj)
    # Start time
    t <- ymd_hms(paste(d_start, t_start), tz = tzone)
    t_next <- t + duration(hour = increment)
    # Get initial set of trajectories
    st <- get_t_window(conn, schema, view, t, interval)
    st_next <- get_t_window(conn, schema, view, t_next, interval)
    
    # Get full traj
    st.1 <- get_full_traj(conn, schema, view)
    
    factpal <- colorFactor(topo.colors(4), st$animal_name)
    
    ui <- bootstrapPage(
        tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
        h3(textOutput("tstamp")),
        tags$script('$(document).on("keydown",
                    function (e) {
                    if(e.which == 66) {
                    Shiny.onInputChange("b", new Date());
                    } else if (e.which == 78) {
                    Shiny.onInputChange("n", new Date());
                    }
                    });
                    '),
        actionButton("b", "Back"),
        actionButton("n", "Next"),
        h5("press B or N"),
        leafletOutput("map", width = "100%", height = "100%")
    )
    
    server <- function(input, output) {
        
        w <- reactiveValues(data = st.1)
        x <- reactiveValues(currStep = st, nextStep = st_next)
        # get current time window and the next
        timeOut <- reactiveValues(currTime = t,
                                  nextTime = t + duration(hour = increment))
        
        # Only update timestamp on click
        observeEvent(input$n, {
            timeOut$currTime <- timeOut$currTime + duration(hour = increment)
            timeOut$nextTime <- timeOut$currTime + duration(hour = increment)
        })
        
        observeEvent(input$b, {
            timeOut$currTime <- timeOut$currTime - duration(hour = increment)
            timeOut$nextTime <- timeOut$currTime - duration(hour = increment)
        })
        
        # Report current timestamp
        output$tstamp <- renderText({
            paste("Current time stamp:", format(timeOut$currTime, usetz = TRUE))
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
                
                # map %>% 
                #     addPolylines(
                #         data = x$currStep,
                #         group = "traj",
                #         fillOpacity = 1,
                #         opacity = 1,
                #         color = ~factpal(animal_name),
                #         weight = 4
                #     )
            }
        })
        
        # get the traj segment on every click, on either n or b
        observe({
            x$currStep <- get_t_window(conn, schema, view, timeOut$currTime, interval)
            x$nextStep <- get_t_window(conn, schema, view, timeOut$nextTime, interval)
        })
        
        observe({
            leafletProxy("map", data = x$currStep, deferUntilFlush = TRUE) %>%
                # clearGroup("traj") %>%
                addPolylines(
                    data = x$currStep,
                    group = "traj",
                    fillOpacity = 1,
                    opacity = 1,
                    color = 'red', #~factpal(animal_name),
                    weight = 4
                ) #%>% 
                # addPolylines(
                #     data = x$nextStep,
                #     group = "trajnew",
                #     fillOpacity = 1,
                #     opacity = 1,
                #     color = 'green', #~factpal(animal_name),
                #     weight = 4
                # ) #%>% 
                # # clearGroup("trajnew")
        })
        
    }
    shinyApp(ui, server)
}


# ltrajPlotter <- function(conn, schema, pgtraj, pgtraj_sf, d_start, t_start, tzone, increment,
#                          nr_increment, interval) {
#     view <- paste0("step_geometry_shiny_", pgtraj)
#     # Start time
#     t <- ymd_hms(paste(d_start, t_start), tz = tzone)
#     t_next <- t + duration(hour = increment)
#     
#     # st.1 <- get_full_traj(conn, schema, view)
#     
#     st <- filter(pgtraj_sf, relocation_time >= t & relocation_time < t + duration(hour = increment))
#     st_next <- filter(pgtraj_sf, relocation_time >= t_next & relocation_time < t_next + duration(hour = increment))
#     
#     factpal <- colorFactor(topo.colors(4), st$animal_name)
#     
#     ui <- bootstrapPage(
#         tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
#         h3(textOutput("tstamp")),
#         tags$script('$(document).on("keydown",
#                     function (e) {
#                     if(e.which == 66) {
#                     Shiny.onInputChange("b", new Date());
#                     } else if (e.which == 78) {
#                     Shiny.onInputChange("n", new Date());
#                     }
#                     });
#                     '),
#         actionButton("b", "Back"),
#         actionButton("n", "Next"),
#         h5("press B or N"),
#         leafletOutput("map", width = "100%", height = "100%")
#     )
#     
#     server <- function(input, output) {
#         
#         # w <- reactiveValues(data = st.1)
#         x <- reactiveValues(currStep = st, nextStep = st_next)
#         # get current time window and the next
#         timeOut <- reactiveValues(currTime = t,
#                                   nextTime = t + duration(hour = increment))
#         
#         # Only update timestamp on click
#         observeEvent(input$n, {
#             timeOut$currTime <- timeOut$currTime + duration(hour = increment)
#             timeOut$nextTime <- timeOut$currTime + duration(hour = increment)
#         })
#         
#         observeEvent(input$b, {
#             timeOut$currTime <- timeOut$currTime - duration(hour = increment)
#             timeOut$nextTime <- timeOut$currTime - duration(hour = increment)
#         })
#         
#         # Report current timestamp
#         output$tstamp <- renderText({
#             paste("Current time stamp:", format(timeOut$currTime, usetz = TRUE))
#         })
#         
#         # Leaflet base map, and starting view centered at the trajectories
#         output$map <- renderLeaflet({
#             # if (is.null(w$data)) {
#             #     return()
#             # } else {
#                 map <- leaflet() %>%
#                     addTiles(group = "OSM (default)") %>%
#                     addPolylines(
#                         data = st,
#                         group = "trajfull",
#                         fillOpacity = .5,
#                         opacity = .5,
#                         color = ~factpal(animal_name),
#                         weight = 2
#                     ) %>%
#                     addLayersControl(
#                         overlayGroups = c("OSM (default)", "trajfull"),
#                         options = layersControlOptions(collapsed = FALSE)
#                     ) 
#             # }
#         })
#         
#         # get the traj segment on every click, on either n or b
#         observe({
#             x$currStep <-
#                 filter(
#                     pgtraj_sf,
#                     relocation_time >= timeOut$currTime &
#                         relocation_time < timeOut$currTime + duration(hour = interval)
#                 )
#             x$nextStep <-
#                 filter(
#                     pgtraj_sf,
#                     relocation_time >= timeOut$nextTime &
#                         relocation_time < timeOut$nextTime + duration(hour = interval)
#                 )
#         })
#         
#         observe({
#             leafletProxy("map", data = x$currStep, deferUntilFlush = TRUE) %>%
#                 clearGroup("traj") %>%
#                 addPolylines(
#                     layerId = "a",
#                     data = x$currStep,
#                     group = "traj",
#                     fillOpacity = 1,
#                     opacity = 1,
#                     color = ~factpal(animal_name),
#                     weight = 4
#                 )
#         })
#         
#     }
#     shinyApp(ui, server)
#     
# }

