library(sf)
library(lubridate)
library(dplyr)
library(DBI)

# Queries ------------------------------------------------------------

#' Get steps within a temporal window
#'
#' @param conn DBI::DBIConnection
#' @param schema String. Schema name.
#' @param view String. View name.
#' @param time String of the start time of the time window. Including time zone.
#' @param interval lubridate::Period object of the time window
#' @param step_mode Boolean. Detailed step info (TRUE) or aggregate
#' @param info_cols Character vector of the infolocs columns of the pgtraj.
#'
#' @return A simple feature object of the steps.
#' 
#' @author Balázs Dukai \email{balazs.dukai@@gmail.com}

get_step_window <- function(conn, schema, view, time, interval, step_mode,
                            info_cols){
    stopifnot(is.period(interval))
    i <- period_to_seconds(interval)
    t <- dbQuoteString(conn, format(time, usetz = TRUE))
    t_interval <- dbQuoteString(conn, paste(i, "seconds"))
    schema_q <- dbQuoteIdentifier(conn, schema)
    view_q <- dbQuoteIdentifier(conn, view)
    
    if(step_mode){
        sql_query <- paste0("
                            SELECT
                                step_id,
                                step_geom,
                                date,
                                dx,
                                dy,
                                dist,
                                dt,
                                abs_angle,
                                rel_angle,
                                ",info_cols,"
                                animal_name,
                                burst_name,
                                pgtraj_name
                            FROM ", schema_q, ".", view_q, " a
                            WHERE a.date >= ",t,"::timestamptz
                            AND a.date < (",t,"::timestamptz + ",
                                t_interval, "::INTERVAL)
                            AND a.step_geom IS NOT NULL;")
    } else {
        sql_query <- paste0("
                            SELECT
                                st_makeline(step_geom)::geometry(
                                    linestring,
                                    4326
                                ) AS step_geom,
                                burst_name,
                                animal_name
                            FROM
                                ", schema_q, ".", view_q, "
                            WHERE
                                date >= ",t,"::timestamptz
                                AND date < (",t,"::timestamptz + ",
                            t_interval, "::INTERVAL)
                            GROUP BY
                                burst_name, animal_name;")
        
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
        sql_array <- paste(burst_name, collapse = "','")
        burst_sql <- paste0("ANY(ARRAY['",sql_array,"'])")
    }
    
    schema_q <- dbQuoteIdentifier(conn, schema)
    view_q <- dbQuoteIdentifier(conn, view)
    
    sql_query <- paste0("
                        SELECT *
                        FROM ", schema_q, ".all_burst_summary_shiny
                        WHERE burst_name = ", burst_sql, ";")
    
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

#' Get default time parameters for steps
#'
#' @param conn DBI::DBIConnection
#' @param schema String. Schema name.
#' @param view String. View name.
#' @param pgtraj String. Pgtraj name
#'
#' @return data frame with columns: tstamp_start, tstamp_last, increment, tzone
#' 
#' @author Balázs Dukai \email{balazs.dukai@@gmail.com}
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
                                MIN( DATE )
                            ) AS tstamp_start,
                            EXTRACT(
                                epoch
                            FROM
                                MAX( DATE )
                            ) AS tstamp_last,
                            PERCENTILE_CONT( 0.5 ) WITHIN GROUP(
                            ORDER BY
                                dt
                            ) AS increment
                        FROM ",schema_q,".", view_q,";")
    
    time_params <- dbGetQuery(conn, sql_query)
    
    return(cbind(time_params, tzone))
}

updateNumericTimeInput <- function(session, inputUnit, inputId, reactiveTime){
    if (inputUnit == "years") {
        updateNumericInput(session, inputId,
                           value = reactiveTime@year)
    } else if (inputUnit == "months") {
        updateNumericInput(session, inputId,
                           value = reactiveTime@month)
    } else if (inputUnit == "days") {
        updateNumericInput(session, inputId,
                           value = reactiveTime@day)
    } else if (inputUnit == "hours") {
        updateNumericInput(session, inputId,
                           value = reactiveTime@hour)
    } else if (inputUnit == "minutes") {
        updateNumericInput(session, inputId,
                           value = reactiveTime@minute)
    } else if (inputUnit == "seconds") {
        updateNumericInput(session, inputId,
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

# layers <- list(c("example_data", "county_subdiv"), c("public", "florida_dem"))
# return: list(name=sf object, name2=sf object)
getLayers <- function(conn, layers) {
    geo_type <- findGeoType(conn, layers)
    base <- list()
    if(length(geo_type$vect) > 0) {
        for(l in seq_along(geo_type$vect)) {
            relation <-  geo_type$vect[[l]]
            # project to EPSG:4326 for simpler handling
            data <- st_read_db(conn, table = relation) %>% 
                st_transform(4326)
            # add layer name
            # attr(data, "name") <- t[2]
            base[relation[2]] <- list(data)
        }
    } else if(length(geo_type$rast) > 0) {
        for(l in seq_along(geo_type$rast)) {
            relation <- geo_type$rast[[l]]
            # data <- pgGetRast(conn, relation)
            # base[relation[2]] <- list(data)
            warning("raster layers not implemented yet")
        }
    }
    
    return(base)
}

# ras <- readGDAL(dsn) # Get your file as SpatialGridDataFrame
# ras2 <- raster(ras,1) # Convert the first Band to Raster
# plot(ras2)
# 
# rast <- pgGetRast(conn, c("public", "florida_dem"))

# layers <- list(c("example_data", "county_subdiv"))
# b <- getLayers(conn, layers)

# layers <- list(c("example_data", "county_subdiv"), c("public", "florida_dem"))
# geo_type$vect[[1]]
findGeoType <- function(conn, layers) {
    expect_true((length(layers) >= 1))
    # geo_type <- data.frame(name = character(), type = character(),
    #                        schema = character(), table = character(),
    #                        stringsAsFactors = FALSE)
    geo_type <- list(vect = list(), rast = list())
    for(i in seq_along(layers)) {
        layer <- layers[[i]]
        v <- isVector(conn, layer)
        r <- isRaster(conn, layer)
        if(v){
            geo_type$vect <- append(geo_type$vect, layers[i])
        } else {
            geo_type$rast <- append(geo_type$rast, layers[i])
        }
    }
    
    return(geo_type)
}


# layer: c(schema, table)
isVector <- function(conn, layer) {
    sql_query <- paste0("SELECT *
                        FROM public.geometry_columns
                        WHERE f_table_schema = ",dbQuoteString(conn, layer[1]),"
                        AND f_table_name = ",dbQuoteString(conn, layer[2]),
                        ";")
    v <- suppressWarnings(dbGetQuery(conn, sql_query))
    if(nrow(v) > 0) {
        return(TRUE)
    } else {
        return(FALSE)
    }
}


# layer: c(schema, table)
isRaster <- function(conn, layer) {
    sql_query <- paste0("SELECT *
                        FROM public.raster_columns
                        WHERE r_table_schema = ",dbQuoteString(conn, layer[1]),"
                        AND r_table_name = ",dbQuoteString(conn, layer[2]),
                        ";")
    r <- suppressWarnings(dbGetQuery(conn, sql_query))
    if(nrow(r) > 0) {
        return(TRUE)
    } else {
        return(FALSE)
    }
}
