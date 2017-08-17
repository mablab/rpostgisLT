context("rposgisLT: shiny-steps")

schema <- "ibex_traj_materialized_bursts"
pgtraj <- "ibex_int_space"
view <- "step_geometry_shiny_ibex_int_space"
# attr(ibex_int_space, "proj4string") <- CRS("+init=epsg:3395")
# ltraj2pgtraj(conn_data, ibex_int_space, schema = "ibex_traj_materialized_bursts",
#              infolocs = TRUE, overwrite = TRUE)
# createShinyStepsView(conn_data, schema, pgtraj)
# createShinyBurstsView(conn_data, schema)

test_that("get time defaults in time window with time zone", {
    skip_if_not(can_con(conn_data), "could not connect to postgis database")
    
    trajdef <-
        get_traj_defaults(conn_data, schema, view = view,
                          pgtraj)
    expect_equal(trajdef$tstamp_start, as.POSIXct("2003-06-01 CEST"))
    expect_equal(trajdef$tstamp_last, as.POSIXct("2003-06-14 16:00:00 CEST"))
    expect_equal(trajdef$increment, 14400)
    expect_equal(trajdef$time_zone, "Europe/Paris")
})

test_that("get_step_window time ranges", {
    skip_if_not(can_con(conn_data), "could not connect to postgis database")
    
    i <- lubridate::period(2, units = "days")
    expect_silent(
        s <-
            get_step_window(
                conn_data,
                schema,
                view = view,
                time = "2003-06-01 CEST",
                i,
                step_mode = FALSE,
                info_cols = NULL,
                tstamp_start = as.POSIXct("2003-06-01 CEST"),
                tstamp_last = as.POSIXct("2003-06-14 16:00:00 CEST")
            )
    )
    expect_equal(length(st_geometry(s)), 4)
    expect_message(
        s_out <-
            get_step_window(
                conn_data,
                schema,
                view = view,
                time = "2003-05-15 CEST",
                i,
                step_mode = FALSE,
                info_cols = NULL,
                tstamp_start = as.POSIXct("2003-06-01 CEST"),
                tstamp_last = as.POSIXct("2003-06-14 16:00:00 CEST")
            ),
        "time window out of range"
    )
    expect_message(
        s_out2 <-
            get_step_window(
                conn_data,
                schema,
                view = view,
                time = "2003-06-15 CEST",
                i,
                step_mode = FALSE,
                info_cols = NULL,
                tstamp_start = as.POSIXct("2003-06-01 CEST"),
                tstamp_last = as.POSIXct("2003-06-14 16:00:00 CEST")
            ),
        "time window out of range"
    )
    expect_silent(
        s_out3 <-
            get_step_window(
                conn_data,
                schema,
                view = view,
                time = "2003-06-14 16:00:00 CEST",
                i,
                step_mode = FALSE,
                info_cols = NULL,
                tstamp_start = as.POSIXct("2003-06-01 CEST"),
                tstamp_last = as.POSIXct("2003-06-14 16:00:00 CEST")
            )
    )
    expect_message(
        s_out4 <-
            get_step_window(
                conn_data,
                schema,
                view = view,
                time = "2003-06-02 CEST",
                interval = lubridate::period(0, units = "days"),
                step_mode = FALSE,
                info_cols = NULL,
                tstamp_start = as.POSIXct("2003-06-01 CEST"),
                tstamp_last = as.POSIXct("2003-06-14 16:00:00 CEST")
            ),
        "time window out of range",
        label = "interval==0"
    )
    expect_silent(
        s_out4 <-
            get_step_window(
                conn_data,
                schema,
                view = view,
                time = "2003-06-02 CEST",
                interval = lubridate::period(c(2, 57, 17.8380000591278),
                                             c("hour", "minute", "second")),
                step_mode = FALSE,
                info_cols = NULL,
                tstamp_start = as.POSIXct("2003-06-01 CEST"),
                tstamp_last = as.POSIXct("2003-06-14 16:00:00 CEST")
            )
    )
    expect_null(s_out)
    expect_null(s_out2)
    expect_equal(length(st_geometry(s_out3)), 2)
})

test_that("get_step_window time input", {
    skip_if_not(can_con(conn_data), "could not connect to postgis database")
    
    i <- lubridate::period(2, units = "days")
    expect_error(
        s_out <-
            get_step_window(
                conn_data,
                schema,
                view = view,
                time = NULL,
                i,
                step_mode = FALSE,
                info_cols = NULL,
                tstamp_start = as.POSIXct("2003-06-01 CEST"),
                tstamp_last = as.POSIXct("2003-06-14 16:00:00 CEST")
            )
    )
    expect_error(
        s_out2 <-
            get_step_window(
                conn_data,
                schema,
                view = view,
                time = "",
                i,
                step_mode = FALSE,
                info_cols = NULL,
                tstamp_start = as.POSIXct("2003-06-01 CEST"),
                tstamp_last = as.POSIXct("2003-06-14 16:00:00 CEST")
            )
    )
    expect_error(
        s_out3 <-
            get_step_window(
                conn_data,
                schema,
                view = view,
                time = NA,
                i,
                step_mode = FALSE,
                info_cols = NULL,
                tstamp_start = as.POSIXct("2003-06-01 CEST"),
                tstamp_last = as.POSIXct("2003-06-14 16:00:00 CEST")
            )
    )
})

test_that("get_step_window step_mode and info_cols input", {
    skip_if_not(can_con(conn_data), "could not connect to postgis database")
    
    i <- lubridate::period(2, units = "days")
    expect_silent(s_out <- get_step_window(
        conn_data,
        schema,
        view = view,
        time = "2003-06-02 CEST",
        interval = i,
        step_mode = TRUE,
        info_cols = "pkey ,",
        tstamp_start = as.POSIXct("2003-06-01 CEST"),
        tstamp_last = as.POSIXct("2003-06-14 16:00:00 CEST")
    ))
    expect_equal(length(s_out), 13)
    expect_silent(s_out2 <- get_step_window(
        conn_data,
        schema,
        view = view,
        time = "2003-06-02 CEST",
        interval = i,
        step_mode = FALSE,
        info_cols = "pkey ,",
        tstamp_start = as.POSIXct("2003-06-01 CEST"),
        tstamp_last = as.POSIXct("2003-06-14 16:00:00 CEST")
    ))
    expect_equal(length(s_out2), 3)
})

test_that("wtf with get_step_window?!", {
    skip_if_not(can_con(conn_data), "could not connect to postgis database")
    
    get_step_window(
        conn_data,
        schema,
        view = view,
        time = "2003-06-02 CEST",
        interval = lubridate::period(c(2, 57, 17.8380000591278),
                                     c("hour", "minute", "second")),
        step_mode = TRUE,
        info_cols = NULL,
        tstamp_start = as.POSIXct("2003-06-01 CEST"),
        tstamp_last = as.POSIXct("2003-06-14 16:00:00 CEST")
    )
})

test_that("getInfolocsTable", {
    skip_if_not(can_con(conn_data), "could not connect to postgis database")
    
    expect_silent(i <- getInfolocsColumns(conn_data, schema, pgtraj))
    expect_equal(i, "pkey ,")
})

rm(schema, pgtraj)