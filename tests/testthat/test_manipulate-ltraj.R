context("rpostgisLT: manipulate-ltraj")

# test_equal() won't work for ltraj-es with infolocs due to the funky infolocs factors...

test_that("missing relocations", {
    skip_if_not(can_con(conn), "could not connect to postgis database")

    expect_true(ltraj2pgtraj(conn, ibex_na))
    expect_message(ibexTest <- pgtraj2ltraj(conn, pgtraj = "ibex_na"),
                   "successfully")
    expect_true(all.equal(ibex_na, ibexTest))
    try(pgtrajDrop(conn, "ibex_na"))
})

test_that("rounding timestamps", {
    skip_if_not(can_con(conn), "could not connect to postgis database")

    expect_true(ltraj2pgtraj(conn, ibex_4h))
    expect_message(ibexTest <- pgtraj2ltraj(conn, pgtraj = "ibex_4h"),
                   "successfully")
    expect_true(all.equal(ibex_4h, ibexTest))
    try(pgtrajDrop(conn, "ibex_4h"))
})

test_that("interpolate ltraj in space", {
    skip_if_not(can_con(conn), "could not connect to postgis database")

    expect_true(ltraj2pgtraj(conn, ibex_int_space, infolocs = TRUE))
    expect_message(ibexTest <- pgtraj2ltraj(conn, pgtraj = "ibex_int_space"),
                   "successfully")
    expect_match(all.equal(ibex_int_space, ibexTest),
                   "Component 4: Component 7: Mean relative difference: 1.090947e-05",
                 all = FALSE) # due to date rounding
    # time rounding causing all.equal == FALSE
    # ibexTest[[1]]$date == ibex[[1]]$date
    # all.equal(as.integer(ibex[[1]]$date),as.integer(ibexTest[[1]]$date)))
    expect_true(pgtrajDrop(conn, "ibex_int_space"))
})

test_that("interpolate ltraj in time", {
    skip_if_not(can_con(conn), "could not connect to postgis database")

    expect_true(ltraj2pgtraj(conn, ibex_int_time, infolocs = TRUE))
    expect_message(ibexTest <- pgtraj2ltraj(conn, pgtraj = "ibex_int_time"),
                   "successfully")
    expect_true(all.equal(ibex_int_time, ibexTest))
    expect_true(pgtrajDrop(conn, "ibex_int_time"))
})

test_that("infolocs name change of step_id", {
    skip_if_not(can_con(conn), "could not connect to postgis database")

    expect_true(ltraj2pgtraj(conn, ibex_int_time, infolocs = TRUE))

    infolocs(ibex_int_time)[[1]]$step_id <- 1
    infolocs(ibex_int_time)[[2]]$step_id <- 1
    infolocs(ibex_int_time)[[3]]$step_id <- 1
    infolocs(ibex_int_time)[[4]]$step_id <- 1
    expect_true(ltraj2pgtraj(conn, ibex_int_time, overwrite = TRUE, infolocs = TRUE))
    expect_message(ibexTest <- pgtraj2ltraj(conn, pgtraj = "ibex_int_time"),
                   "successfully")
    expect_match(all.equal(ibex_int_time, ibexTest),
                   "Component 4: Attributes: < Component “infolocs”: Names: 1 string mismatch >",
                   all = FALSE)
    # Infolocs name step_id is changed due to conflict
    expect_true(pgtrajDrop(conn, "ibex_int_time"))
})

test_that("add infolocs column DB manually", {
    skip_if_not(can_con(conn), "could not connect to postgis database")

    expect_true(ltraj2pgtraj(conn, ibex_int_time, infolocs = TRUE))
    rpostgis::dbColumn(conn,
                       c("traj", "infolocs_ibex_int_time"),
                       colname = "test",
                       coltype = "text")
    DBI::dbExecute(conn, "UPDATE traj.infolocs_ibex_int_time SET test = 'foo';")
    expect_message(ibexTest <- pgtraj2ltraj(conn, pgtraj = "ibex_int_time"),
                   "successfully")
    expect_match(all.equal(ibex_int_time, ibexTest),
                   "Component 4: Attributes: < Component “infolocs”: Length mismatch: comparison on first 1 components >",
                   all = FALSE)
    # Infolocs name step_id is changed due to conflict, manually added column 'test' is imported
    expect_true(pgtrajDrop(conn, "ibex_int_time"))
})
