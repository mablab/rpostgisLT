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

test_that("get_step_window returns correct time zone", {
    skip_if_not(can_con(conn_data), "could not connect to postgis database")
    
})



rm(schema, pgtraj)