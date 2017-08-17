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
        getTrajDefaults(conn_data, schema, view = view,
                          pgtraj)
    expect_equal(trajdef$tstamp_start, as.POSIXct("2003-06-01 CEST"))
    expect_equal(trajdef$tstamp_last, as.POSIXct("2003-06-14 14:25:39 CEST"))
    expect_equal(trajdef$increment, 4534)
    expect_equal(trajdef$time_zone, "Europe/Paris")
})

test_that("getInfolocsTable", {
    skip_if_not(can_con(conn_data), "could not connect to postgis database")
    
    expect_silent(i <- getInfolocsColumns(conn_data, schema, pgtraj))
    expect_equal(i, "pkey ,")
})

rm(schema, pgtraj, view)