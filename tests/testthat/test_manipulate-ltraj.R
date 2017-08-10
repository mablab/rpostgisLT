context("rpostgisLT: manipulate-ltraj")

test_that("missing relocations", {
    skip_if_not(can_con(conn), "could not connect to postgis database")
    
    refda <- strptime("2003-06-01 00:00", "%Y-%m-%d %H:%M",
                      tz = "Europe/Paris")
    ibex <- setNA(ibex, refda, 4, units = "hour")
    
    expect_true(ltraj2pgtraj(conn, ibex, overwrite = TRUE))
    expect_message(ibexTest <- pgtraj2ltraj(conn, pgtraj = "ibex"),
                   "successfully")
    expect_equal(ibex, ibexTest)
    try(rpostgis::dbDrop(conn, "traj", type = "schema", cascade = TRUE,
                         display = FALSE),
        silent = TRUE)
})

test_that("rounding timestamps", {
    skip_if_not(can_con(conn), "could not connect to postgis database")
    
    refda <- strptime("2003-06-01 00:00", "%Y-%m-%d %H:%M",
                      tz = "Europe/Paris")
    ibex <- sett0(ibex, refda, 4, units = "hour")
    ibex.ref <- ibex
    expect_true(ltraj2pgtraj(conn, ibex, overwrite = TRUE))
    expect_message(ibexTest <- pgtraj2ltraj(conn, pgtraj = "ibex"),
                   "successfully")
    expect_equal(ibex, ibexTest)
    try(rpostgis::dbDrop(conn, "traj", type = "schema", cascade = TRUE,
                         display = FALSE),
        silent = TRUE)
})

