context("rpostgisLT: parameters")

test_that("insert ibex with recomputed parameters", {
    skip_if_not(can_con(conn_data), "could not connect to postgis database")
    ibex_dl <- adehabitatLT::ld(ibex)
    rpostgis::pgInsert(
        conn_data,
        name = c("example_data", "ibex"),
        data.obj = ibex_dl,
        df.mode = TRUE
    ) ## df mode converts times correctly
    
    expect_true(
        as_pgtraj(
            conn_data,
            schema = "traj",
            relocations_table = c("example_data", "ibex"),
            pgtrajs = "ibex",
            animals = "id",
            bursts = "burst",
            tzone = "Europe/Paris",
            relocations = c("x", "y"),
            timestamps = "date",
            rids = ".db_pkid",
            # this is df.mode primary key default name
            srid = 3395
        )
    )
    expect_message(ibex_re <- pgtraj2ltraj(conn_data, "ibex"),
                   "successfully")
    expect_match(all.equal(ibex, ibex_re),
                 "projargs")
    
    try(pgtrajDrop(conn_data, schema = "traj", pgtraj = "ibex",
                   full_clean = TRUE))
    try(rpostgis::dbDrop(
        conn_data,
        name = c("example_data", "ibex"),
        type = "table",
        ifexists = TRUE
    ))
})

test_that("insert albatross with recomputed parameters", {
    skip_if_not(can_con(conn_data), "could not connect to postgis database")
    albatross_dl <- ld(albatross)
    pgInsert(
        conn_data,
        name = c("example_data", "albatross"),
        data.obj = albatross_dl,
        df.mode = TRUE
    ) ## df mode converts times correctly
    
    expect_true(
        as_pgtraj(
            conn_data,
            schema = "traj",
            relocations_table = c("example_data", "albatross"),
            pgtraj = "albatross",
            animals = "id",
            bursts = "burst",
            relocations = c("x", "y"),
            timestamps = "date",
            tzone = "UTC",
            rid = ".db_pkid",
            srid = 3395
        )
    )
    expect_message(albatross_re <- pgtraj2ltraj(conn_data, "albatross"),
                   "successfully")
    expect_match(all.equal(albatross, albatross_re),
                 "projargs")
    
    try(pgtrajDrop(conn_data, schema = "traj", pgtraj = "albatross",
                   full_clean = TRUE))
    try(rpostgis::dbDrop(
        conn_data,
        name = c("example_data", "albatross"),
        type = "table",
        ifexists = TRUE
    ))
})

test_that("null timestamp, relocations x,y, note", {
    skip_if_not(can_con(conn_data), "could not connect to postgis database")
    albatross_dl <- ld(albatross)
    pgInsert(
        conn_data,
        name = c("example_data", "albatross"),
        data.obj = albatross_dl,
        df.mode = TRUE
    ) ## df mode converts times correctly
    
    expect_true(
        as_pgtraj(
            conn_data,
            schema = "traj",
            relocations_table = c("example_data", "albatross"),
            pgtraj = "albatross_type_I",
            animals = "id",
            bursts = NULL,
            relocations = c("x", "y"),
            timestamps = NULL,
            rid = ".db_pkid",
            srid = 3395,
            note = "albatross type I"
        )
    )
    expect_message(albatross_re <- pgtraj2ltraj(conn_data, "albatross_type_I"),
                   "successfully")
    
    try(pgtrajDrop(conn_data, schema = "traj", pgtraj = "albatross_type_I",
                   full_clean = TRUE))
    try(rpostgis::dbDrop(
        conn_data,
        name = c("example_data", "albatross"),
        type = "table",
        ifexists = TRUE
    ))
})
