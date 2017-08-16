context("rposgisLT: shiny-steps")

schema <- "ibex_traj_materialized_bursts"
pgtraj <- "ibex"

test_that("get time defaults in time window with time zone", {
    skip_if_not(can_con(conn_data), "could not connect to postgis database")
    
    trajdef <-
        get_traj_defaults(conn_data, schema, view = "step_geometry_shiny_ibex",
                          pgtraj)
    expect_equal(trajdef$tstamp_start, as.POSIXct("2003-06-01 CEST"))
    expect_equal(trajdef$tstamp_last, as.POSIXct("2003-06-14 16:00:00 CEST"))
    expect_equal(trajdef$increment, 14400)
    expect_equal(trajdef$time_zone, "Europe/Paris")
})

test_that("get_step_window", {
    skip_if_not(can_con(conn_data), "could not connect to postgis database")
    
    i <- lubridate::period(2, units = "days")
    expect_silent(
        s <-
            get_step_window(
                conn_data,
                schema,
                view = "step_geometry_shiny_ibex",
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
                view = "step_geometry_shiny_ibex",
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
                view = "step_geometry_shiny_ibex",
                time = "2003-06-15 CEST",
                i,
                step_mode = FALSE,
                info_cols = NULL,
                tstamp_start = as.POSIXct("2003-06-01 CEST"),
                tstamp_last = as.POSIXct("2003-06-14 16:00:00 CEST")
            ),
        "time window out of range"
    )
    expect_null(s_out)
    expect_null(s_out2)
})

rm(schema, pgtraj)