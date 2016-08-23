library(RPostgreSQL)
library(adehabitatLT)
library(rpostgisLT)
source("./rpostgisLT/utility/utility_functions.R")

# connect to rpostgis database
#cs()
drv <- dbDriver("PostgreSQL")
conn <- dbConnect(drv, user="bdukai", password="ERBAgoNd1#", dbname="rpostgisLT",
        host="localhost")

data(puechcirc)
data(albatross)
data(ibexraw)
data(porpoise)

# update datasets with projection attributes
puechcirc <- dl(ld(puechcirc))
albatross <- dl(ld(albatross))
ibexraw <- dl(ld(ibexraw))
porpoise <- dl(ld(porpoise))

## THESE GIVE ERROR
# insert 1. puechcirc 2. albatross
ltraj2pgtraj(conn, ltraj = puechcirc, comment = "test CRS on puechcirc")
ltraj2pgtraj(conn, ltraj = albatross, comment = "test CRS on albatross")

puechcirc_re <- pgtraj2ltraj(conn, schema = 'traj', pgtraj = 'puechcirc') # OK
albatross_re <- pgtraj2ltraj(conn, schema = 'traj', pgtraj = 'albatross') # ERROR

all.equal(puechcirc, puechcirc_re)

dbSendQuery(conn, "DROP SCHEMA traj CASCADE;")
rm(puechcirc_re, albatross_re)

# insert 1. albatross 2. puechcirc
ltraj2pgtraj(conn, ltraj = albatross, comment = "test CRS on albatross")
ltraj2pgtraj(conn, ltraj = puechcirc, comment = "test CRS on puechcirc")

albatross_re <- pgtraj2ltraj(conn, schema = 'traj', pgtraj = 'albatross') # OK
puechcirc_re <- pgtraj2ltraj(conn, schema = 'traj', pgtraj = 'puechcirc') # ERROR

all.equal(albatross, albatross_re)

dbSendQuery(conn, "DROP SCHEMA traj CASCADE;")
rm(puechcirc_re, albatross_re)

## THESE DON'T GIVE ERROR in any combination
ltraj2pgtraj(conn, ltraj = porpoise, comment = "test CRS on porpoise")
ltraj2pgtraj(conn, ltraj = ibexraw, comment = "test CRS on ibexraw")

ibexraw_re <- pgtraj2ltraj(conn, schema = 'traj', pgtraj = 'ibexraw')
porpoise_re <- pgtraj2ltraj(conn, schema = 'traj', pgtraj = 'porpoise')

all.equal(ibexraw, ibexraw_re)
all.equal(porpoise, porpoise_re)

dbSendQuery(conn, "DROP SCHEMA traj CASCADE;")
rm(ibexraw_re, porpoise_re)

# or if the the two problematic datasets are inserted alone
ltraj2pgtraj(conn, ltraj = puechcirc, comment = "test CRS on puechcirc")
puechcirc_re <- pgtraj2ltraj(conn, schema = 'traj', pgtraj = 'puechcirc')

all.equal(puechcirc, puechcirc_re)

dbSendQuery(conn, "DROP SCHEMA traj CASCADE;")
rm(puechcirc_re)

ltraj2pgtraj(conn, ltraj = albatross, comment = "test CRS on albatross")
albatross_re <- pgtraj2ltraj(conn, schema = 'traj', pgtraj = 'albatross')

all.equal(albatross, albatross_re)

dbSendQuery(conn, "DROP SCHEMA traj CASCADE;")
rm(albatross_re)


