library(rpostgisLT)

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