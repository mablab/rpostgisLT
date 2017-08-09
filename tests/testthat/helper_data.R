# library(rpostgisLT)

# test environment
e <- test_env()

### Get test datasets
data(ibex, package = "adehabitatLT", envir = e)
data(ibexraw, package = "adehabitatLT", envir = e)
data(puechcirc, package = "adehabitatLT", envir = e)
data(albatross, package = "adehabitatLT", envir = e)
data(porpoise, package = "adehabitatLT", envir = e)

### Update ltraj with 'proj4string' attribute
ibex <- rec(get("ibex", envir = e))
ibexraw <- rec(get("ibexraw", envir = e))
puechcirc <- rec(get("puechcirc", envir = e))
albatross <- rec(get("albatross", envir = e))
porpoise <- rec(get("porpoise", envir = e))

ibex_srs <- ibex
ibexraw_srs <- ibexraw
puechcirc_srs <- puechcirc
albatross_srs <- albatross
porpoise_srs <- porpoise

### Create Type I ltraj
ibexraw_I <- typeII2typeI(ibexraw)
albatross_I <- typeII2typeI(albatross)
porpoise_I <- typeII2typeI(porpoise)

ibexraw_I_srs <- typeII2typeI(ibexraw)
albatross_I_srs <- typeII2typeI(albatross)
porpoise_I_srs <- typeII2typeI(porpoise)

### Set some projection for testing
srs <- CRS("+init=epsg:3395")
srs2 <- CRS("+init=epsg:4326")

attr(ibexraw_srs, 'proj4string') <- srs
attr(puechcirc_srs, 'proj4string') <- srs2
attr(albatross_srs, 'proj4string') <- srs
attr(porpoise_srs, 'proj4string') <- srs2
### Type I
attr(porpoise_I_srs, 'proj4string') <- srs2
attr(albatross_I_srs, 'proj4string') <- srs
attr(ibexraw_I_srs, 'proj4string') <- srs
