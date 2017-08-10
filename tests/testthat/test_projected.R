context("rpostgisLT: projected")

test_that("test CRS on ibexraw", {
    skip_if_not(can_con(conn), "could not connect to postgis database")
    expect_true(ltraj2pgtraj(
        conn,
        ltraj = ibexraw_srs,
        note = "test CRS on ibexraw",
        overwrite = TRUE
    ))
    expect_message(
        ibexraw_re <-
            pgtraj2ltraj(conn, schema = 'traj', pgtraj = 'ibexraw_srs'),
        "successfully")
    expect_equal(ibexraw_srs, ibexraw_re)
})

test_that("test CRS on puechcirc", {
    skip_if_not(can_con(conn), "could not connect to postgis database")
    expect_true(
        ltraj2pgtraj(
            conn,
            ltraj = puechcirc_srs,
            note = "test CRS on puechcirc",
            overwrite = TRUE
        )
    )
    expect_message(
        puechcirc_re <-
            pgtraj2ltraj(conn, schema = 'traj', pgtraj = 'puechcirc_srs'),
        "successfully"
    )
    expect_equal(puechcirc_srs, puechcirc_re)
})

test_that("test CRS on albatross", {
    skip_if_not(can_con(conn), "could not connect to postgis database")
    expect_true(
        ltraj2pgtraj(
            conn,
            ltraj = albatross_srs,
            note = "test CRS on albatross",
            overwrite = TRUE
        )
    )
    expect_message(
        albatross_re <-
            pgtraj2ltraj(conn, schema = 'traj', pgtraj = 'albatross_srs'),
        "successfully"
    )
    expect_equal(albatross_srs, albatross_re)
})

test_that("test CRS on porpoise", {
    skip_if_not(can_con(conn), "could not connect to postgis database")
    expect_true(ltraj2pgtraj(
        conn,
        ltraj = porpoise_srs,
        note = "test CRS on porpoise",
        overwrite = TRUE
    ))
    expect_message(
        porpoise_re <-
            pgtraj2ltraj(conn, schema = 'traj', pgtraj = 'porpoise_srs'),
        "successfully"
    )
    expect_equal(porpoise_srs, porpoise_re)
})

test_that("pgtrajDrop full clean with default schema", {
    skip_if_not(can_con(conn), "could not connect to postgis database")
    expect_true(pgtrajDrop(conn))
})

test_that("porpoise type I arbitrary crs", {
    skip_if_not(can_con(conn), "could not connect to postgis database")
    expect_true(ltraj2pgtraj(conn,
                             ltraj = porpoise_I_srs,
                             schema = "type_I",
                             note = "arbitrary CRS"))
    expect_message(
        porpoise_I_re <-
            pgtraj2ltraj(conn, schema = "type_I", pgtraj = "porpoise_I_srs"),
        "successfully"
    )
    expect_equal(porpoise_I_srs, porpoise_I_re)
})

test_that("albatross type I arbitrary crs", {
    skip_if_not(can_con(conn), "could not connect to postgis database")
    expect_true(ltraj2pgtraj(conn,
                 ltraj = albatross_I_srs,
                 schema = "type_I",
                 note = "arbitrary CRS"))
    expect_message(
        albatross_I_re <-
            pgtraj2ltraj(conn, schema = "type_I", pgtraj = "albatross_I_srs"),
        "successfully"
    )
    expect_equal(albatross_I_srs, albatross_I_re)
})

test_that("ibexraw type I arbitrary crs", {
    skip_if_not(can_con(conn), "could not connect to postgis database")
    expect_true(ltraj2pgtraj(conn,
                 ltraj = ibexraw_I_srs,
                 schema = "type_I",
                 note = "arbitrary CRS"))
    expect_message(
        ibexraw_I_re <-
            pgtraj2ltraj(conn, schema = "type_I", pgtraj = "ibexraw_I_srs"),
        "successfully"
    )
    expect_equal(ibexraw_I_srs, ibexraw_I_re)
})

