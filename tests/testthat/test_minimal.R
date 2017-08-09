context("rpostgisLT: minimal")

### Postgres
can_con <- function(x) inherits(x, "PostgreSQLConnection")
pg <- NULL
test_that("check utils", expect_false(can_con(pg)))
try(conn <- RPostgreSQL::dbConnect(RPostgreSQL::PostgreSQL(),
                                 dbname = "rpglt_empty"), silent=TRUE)

### Get test datasets
data(ibex)
data(ibexraw)
data(puechcirc)
data(albatross)
data(porpoise)

### Update ltraj with 'proj4string' attribute
ibex <- rec(ibex)
ibexraw <- rec(ibexraw)
puechcirc <- rec(puechcirc)
albatross <- rec(albatross)
porpoise <- rec(porpoise)

### Create Type I ltraj
ibexraw_I <- typeII2typeI(ibexraw)
albatross_I <- typeII2typeI(albatross)
porpoise_I <- typeII2typeI(porpoise)

### Set some projection for testing
srs <- CRS("+init=epsg:3395")
srs2 <- CRS("+init=epsg:4326")

##############################################################################
### Minimal test

test_that("minimal ltraj transfer is equal and identical", {
    skip_if_not(can_con(conn), "could not connect to postgis database")
    
    ib_min <-
        dl(ld(ibexraw[1])[1:10,]) # note that step parameters are recomputed on purpose
    rpostgisLT::ltraj2pgtraj(conn,
                 ltraj = ib_min,
                 schema = "traj_min",
                 pgtraj = "ib_min")
    ib_min_re <-
        rpostgisLT::pgtraj2ltraj(conn, schema = "traj_min", pgtraj = "ib_min")
    expect_true(all.equal(ib_min, ib_min_re))
    expect_true(identical(ib_min, ib_min_re))
})


# 
# ### overwrite fail test
# try(ltraj2pgtraj(conn,
#                  ltraj = ib_min,
#                  schema = "traj_min",
#                  pgtraj = "ib_min"))
# 
# ### null proj4string test
# attr(ib_min, "proj4string") <- NULL
# ltraj2pgtraj(
#     conn,
#     ltraj = ib_min,
#     schema = "traj_min",
#     pgtraj = "ib_min",
#     overwrite = TRUE
# )
# 
# dbDrop(conn, "traj_min", type = "schema", cascade = TRUE)
# rm(ib_min_re, ib_min)
# 
# ib_min_srs <- dl(ld(ibexraw[2])[1:10,], proj4string = srs)
# # note that step parameters are recomputed on purpose
# ltraj2pgtraj(conn,
#              ltraj = ib_min_srs,
#              schema = "traj_min",
#              pgtraj = "ib_min_3395")
# ib_min_srs_re <-
#     pgtraj2ltraj(conn, schema = "traj_min", pgtraj = "ib_min_3395")
# all.equal(ib_min_srs, ib_min_srs_re)
# Sys.sleep(2)
# 
# dbDrop(conn, "traj_min", type = "schema", cascade = TRUE)
# rm(ib_min_srs, ib_min_srs_re)
# 
# 
# ### Basic ltraj
# ibexraw                                 # No infolocs in ibexraw.
# is.regular(ibexraw)
# # FALSE
# 
# ltraj2pgtraj(conn, ibex, overwrite = TRUE) # Default should be in schema
# # 'traj' and use ltraj name
# # ('ibex') as pgtraj name.
# ibexTest <-
#     pgtraj2ltraj(conn, pgtraj = "ibex")     # Default should look into
# # 'traj' schema.
# all.equal(ibex, ibexTest)
# Sys.sleep(2)
# 
# dbDrop(conn, "traj", type = "schema", cascade = TRUE)
# rm(ibexTest)
# 
# ### More basic ltraj
# 
# ## Set projections for testing
# attr(ibexraw, 'proj4string') <- srs
# attr(puechcirc, 'proj4string') <- srs2
# attr(albatross, 'proj4string') <- srs
# attr(porpoise, 'proj4string') <- srs2
# ## Type I
# attr(porpoise_I, 'proj4string') <- srs2
# attr(albatross_I, 'proj4string') <- srs
# attr(ibexraw_I, 'proj4string') <- srs
# 
# ltraj2pgtraj(conn,
#              ltraj = ibexraw,
#              note = "test CRS on ibexraw",
#              overwrite = TRUE)
# ltraj2pgtraj(conn,
#              ltraj = puechcirc,
#              note = "test CRS on puechcirc",
#              overwrite = TRUE)
# ltraj2pgtraj(conn,
#              ltraj = albatross,
#              note = "test CRS on albatross",
#              overwrite = TRUE)
# ltraj2pgtraj(conn,
#              ltraj = porpoise,
#              note = "test CRS on porpoise",
#              overwrite = TRUE)
# 
# ## pgtrajDrop test
# ltraj2pgtraj(conn,
#              ltraj = porpoise,
#              note = "test CRS on porpoise",
#              overwrite = TRUE)
# pgtrajDrop(conn)
# 
# ## Type I
# ltraj2pgtraj(conn,
#              ltraj = porpoise_I,
#              schema = "type_I",
#              note = "arbitrary CRS")
# ltraj2pgtraj(conn,
#              ltraj = albatross_I,
#              schema = "type_I",
#              note = "arbitrary CRS")
# ltraj2pgtraj(conn,
#              ltraj = ibexraw_I,
#              schema = "type_I",
#              note = "arbitrary CRS")
# 
# ibexraw_re <-
#     pgtraj2ltraj(conn, schema = 'traj', pgtraj = 'ibexraw')
# puechcirc_re <-
#     pgtraj2ltraj(conn, schema = 'traj', pgtraj = 'puechcirc')
# albatross_re <-
#     pgtraj2ltraj(conn, schema = 'traj', pgtraj = 'albatross')
# porpoise_re <-
#     pgtraj2ltraj(conn, schema = 'traj', pgtraj = 'porpoise')
# ## Type I
# porpoise_I_re <-
#     pgtraj2ltraj(conn, schema = "type_I", pgtraj = "porpoise_I")
# albatross_I_re <-
#     pgtraj2ltraj(conn, schema = "type_I", pgtraj = "albatross_I")
# ibexraw_I_re <-
#     pgtraj2ltraj(conn, schema = "type_I", pgtraj = "ibexraw_I")
# 
# ### Testing for equality
# all.equal(ibexraw, ibexraw_re)
# all.equal(puechcirc, puechcirc_re)
# all.equal(albatross, albatross_re)
# all.equal(porpoise, porpoise_re)
# ## Type I
# all.equal(ibexraw_I, ibexraw_I_re)
# all.equal(porpoise_I, porpoise_I_re)
# all.equal(albatross_I, albatross_I_re)
# Sys.sleep(2)
# 
# 
# ### Clean up
# dbDrop(conn, "traj", type = "schema", cascade = TRUE)
# dbDrop(conn, "type_I", type = "schema", cascade = TRUE)
# rm(
#     ibexraw_re,
#     puechcirc_re,
#     albatross_re,
#     porpoise_re,
#     ibexraw_I_re,
#     albatross_I_re,
#     porpoise_I_re
# )
