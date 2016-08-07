## Establish connection with rpostgisLT database
source("./rpostgisLT/utility/utility_functions.R")
cs() # creates globals conn and drv


data(ibexraw)
#data(ibex)
data(puechcirc)
data(albatross)
data(porpoise)

## Update the ltrajs with proj4string
x <- ld(ibexraw)
ibexraw <- dl(x)

x <- ld(puechcirc)
puechcirc <- dl(x)

x <- ld(albatross)
albatross <- dl(x)

x <- ld(porpoise)
porpoise <- dl(x)


## Minimal test
# without SRS
ib_min <- dl(ld(ibexraw[1])[1:10, ]) # note that step parameters are recomputed on purpose
ltraj2pgtraj(conn, schema = "traj_min", ltraj = ib_min, pgtraj = "ib_min")
ib_min_re <- pgtraj2ltraj(conn, schema = "traj_min", pgtraj = "ib_min")
all.equal(ib_min, ib_min_re)
dbSendQuery(conn, "DROP SCHEMA traj_min CASCADE;")

# with SRS
srs <- CRS("+init=epsg:3395")
ib_min_srs <- dl(ld(ibexraw[2])[1:10, ], proj4string = srs) # note that step 
# parameters are recomputed on purpose
ltraj2pgtraj(conn, schema = "traj_min", ltraj = ib_min_srs, pgtraj = "ib_min_3395")
ib_min_srs_re <- pgtraj2ltraj(conn, schema = "traj_min", pgtraj = "ib_min_3395")
all.equal(ib_min_srs, ib_min_srs_re)
dbSendQuery(conn, "DROP SCHEMA traj_min CASCADE;")


## Basic ltraj
ibexraw                                 # No infolocs in ibexraw.
is.regular(ibexraw)
## FALSE

ltraj2pgtraj(conn, ibexraw)                   # Default should be in schema
                                        # 'traj' and use ltraj name
                                        # ('ibex') as pgtraj name.
ibexrawTest <- pgtraj2ltraj(conn, pgtraj = "ibexraw")     # Default should look into
                                        # 'traj' schema.
all.equal(ibexraw, ibexrawTest)
## TRUE
dbSendQuery(conn, "DROP SCHEMA traj CASCADE;")

## Import ltraj by its parts
for (i in 1:length(ibexraw)) {
    x <- dl(ld(ibexraw[i]))
    pgtraj <- paste0("ibexraw", i)
    ltraj2pgtraj(conn, ltraj = x, schema = "traj", pgtraj = pgtraj)
    x_re <- pgtraj2ltraj(conn, schema = "traj", pgtraj = pgtraj)
    attr(x_re, "regular") <- FALSE
    print(paste("all.equal() result:",all.equal(x, x_re)))
}

# FIXME gives schema error because there are duplicate ids -> one animal has two bursts
for (i in 1:length(puechcirc)) {
    x <- dl(ld(puechcirc[i]))
    pgtraj <- paste0("puechcirc", i)
    ltraj2pgtraj(conn, ltraj = x, schema = "traj", pgtraj = pgtraj)
    x_re <- pgtraj2ltraj(conn, schema = "traj", pgtraj = pgtraj)
    attr(x_re, "regular") <- TRUE
    print(paste("all.equal() result:",all.equal(x, x_re)))
}

for (i in 1:length(albatross)) {
    x <- dl(ld(albatross[i]))
    pgtraj <- paste0("albatross", i)
    ltraj2pgtraj(conn, ltraj = x, schema = "traj", pgtraj = pgtraj)
    x_re <- pgtraj2ltraj(conn, schema = "traj", pgtraj = pgtraj)
    attr(x_re, "regular") <- FALSE
    print(paste("all.equal() result:",all.equal(x, x_re)))
}

# FIXME porpoise[3] is $regular TRUE, while others are FALSE
for (i in 1:length(porpoise)) {
    x <- dl(ld(porpoise[i]))
    pgtraj <- paste0("porpoise", i)
    ltraj2pgtraj(conn, ltraj = x, schema = "traj", pgtraj = pgtraj)
    x_re <- pgtraj2ltraj(conn, schema = "traj", pgtraj = pgtraj)
    attr(x_re, "regular") <- FALSE
    print(paste("all.equal() result:",all.equal(x, x_re)))
}
dbSendQuery(conn, "DROP SCHEMA traj CASCADE;")


## More basic ltraj without SRS
ltraj2pgtraj(conn, ltraj = ibexraw, comment = "test CRS on ibexraw")
ltraj2pgtraj(conn, ltraj = puechcirc, comment = "test CRS on puechcirc")
ltraj2pgtraj(conn, ltraj = albatross, comment = "test CRS on albatross")
ltraj2pgtraj(conn, ltraj = porpoise, comment = "test CRS on porpoise")

ibexraw_re <- pgtraj2ltraj(conn, pgtraj = 'ibexraw')
puechcirc_re <- pgtraj2ltraj(conn, pgtraj = 'puechcirc')
albatross_re <- pgtraj2ltraj(conn, pgtraj = 'albatross')
porpoise_re <- pgtraj2ltraj(conn, pgtraj = 'porpoise')

all.equal(ibexraw, ibexraw_re)
all.equal(puechcirc, puechcirc_re)
all.equal(albatross, albatross_re)
all.equal(porpoise, porpoise_re)

dbSendQuery(conn, "DROP SCHEMA traj CASCADE;")
rm(ibexraw_re, puechcirc_re, albatross_re, porpoise_re)


## More basic ltraj with SRS
srs2 <- CRS("+init=epsg:4326")
attr(ibexraw, 'proj4string') <- srs
attr(puechcirc, 'proj4string') <- srs2
attr(albatross, 'proj4string') <- srs
attr(porpoise, 'proj4string') <- srs2

ltraj2pgtraj(conn, ltraj = ibexraw, comment = "test CRS on ibexraw")
ltraj2pgtraj(conn, ltraj = puechcirc, comment = "test CRS on puechcirc")
ltraj2pgtraj(conn, ltraj = albatross, comment = "test CRS on albatross")
ltraj2pgtraj(conn, ltraj = porpoise, comment = "test CRS on porpoise")

ibexraw_re <- pgtraj2ltraj(conn, pgtraj = 'ibexraw')
puechcirc_re <- pgtraj2ltraj(conn, pgtraj = 'puechcirc')
albatross_re <- pgtraj2ltraj(conn, pgtraj = 'albatross')
porpoise_re <- pgtraj2ltraj(conn, pgtraj = 'porpoise')

all.equal(ibexraw, ibexraw_re)
all.equal(puechcirc, puechcirc_re)
all.equal(albatross, albatross_re)
all.equal(porpoise, porpoise_re)

dbSendQuery(conn, "DROP SCHEMA traj CASCADE;")


## Missing relocations
refda <- strptime("2003-06-01 00:00", "%Y-%m-%d %H:%M",
    tz = "Europe/Paris")
(ibex <- setNA(ibex, refda, 4, units = "hour"))
ltraj2pgtraj(conn, ibex, overwrite = TRUE)
ibexTest <- pgtraj2ltraj(conn, pgtraj = "ibex")
all.equal(ibex, ibexTest)
## TRUE


## Rounding timestamps
(ibex <- sett0(ibex, refda, 4, units = "hour"))
ibex.ref <- ibex                        # At this stage, 'ibex' is our
                                        # reference data
ltraj2pgtraj(conn, ibex, overwrite = TRUE)
ibexTest <- pgtraj2ltraj(conn, pgtraj = "ibex")
all.equal(ibex, ibexTest)
## TRUE


## Interpolation

## 1. In space
summary(ld_opt(ibex)$dist)
(ibex <- redisltraj(ibex, 400))         # Note that 'redisltraj'
                                        # creates an 'infolocs'
                                        # attribute, which we remove
                                        # for now:
ibex <- removeinfo(ibex)
ltraj2pgtraj(conn, ibex, overwrite = TRUE)
ibexTest <- pgtraj2ltraj(conn, pgtraj = "ibex")
all.equal(ibex, ibexTest)
## TRUE

## 2. In time
ibex <- ibex.ref
(ibex <- redisltraj(na.omit(ibex), 14400, type = "time"))
ibex <- removeinfo(ibex)
ltraj2pgtraj(conn, ibex, overwrite = TRUE)
ibexTest <- pgtraj2ltraj(conn, pgtraj = "ibex")
all.equal(ibex, ibexTest)
## TRUE


## Subset

## 1. Subset on given parameters
ibex <- ibex.ref
## We work on the data frame from the trajectory, which we subset, and
## then rebuild the ltraj without recomputing trajectory parameters;
## this is essentially what 'hab::subset' does.
## Note that the steps are not discontinuous any more.
ibex <- ld_opt(ibex)
ibex <- droplevels(ibex[ibex$dist < 400 & !is.na(ibex$dist), ])
ibex <- dl_opt(ibex)
head(ibex[[1]])
ltraj2pgtraj(conn, ibex, overwrite = TRUE)
ibexTest <- pgtraj2ltraj(conn, pgtraj = "ibex")
all.equal(ibex, ibexTest)

## 2. Subsample on the temporal sequence
ibex <- ibex.ref
(ibex <- subsample(ibex, 14400*2))
ltraj2pgtraj(conn, ibex, overwrite = TRUE)
ibexTest <- pgtraj2ltraj(conn, pgtraj = "ibex")
all.equal(ibex, ibexTest)


## Cut, bind bursts

## 1. Cut if there is a step greater than 3000 m
ibex <- ibex.ref
(ibex <- cutltraj(ibex, "dist > 3000"))
ltraj2pgtraj(conn, ibex, overwrite = TRUE)
ibexTest <- pgtraj2ltraj(conn, pgtraj = "ibex")
all.equal(ibex, ibexTest)

## 2. Bind back by individual:
(ibex <- bindltraj(ibex))
ibex <- removeinfo(ibex)
ltraj2pgtraj(conn, ibex, overwrite = TRUE)
ibexTest <- pgtraj2ltraj(conn, pgtraj = "ibex")
all.equal(ibex, ibexTest)


## Combine trajectories
ibex <- ibex.ref
ibex2 <- ibex
burst(ibex2) <- paste(burst(ibex2), "2", sep = "-")
(ibex <- c(ibex, ibex2)[order(id(c(ibex, ibex2)))])
ltraj2pgtraj(conn, ibex, overwrite = TRUE)
ibexTest <- pgtraj2ltraj(conn, pgtraj = "ibex")
all.equal(ibex, ibexTest)

##############################################################################
## Test input from database
## NOTE: connection to the host: basille-flrec.ad.ufl.edu, database: rpostgis required
as_pgtraj(conn, 
        schema = "traj_t2",
        relocations_table = "example_data.relocations_plus",
        pgtrajs = "id",
        animals = "animal",
        bursts = "burst",
        relocations = "geom",
        timestamps = "time",
        rid = "gid")

# variables provided manually
as_pgtraj(conn, 
        schema = "traj_t2",
        relocations_table = "example_data.reloc_medium", 
        pgtrajs = "medium2",
        animals = "sea turtle",
        relocations = "geom",
        timestamps = "time",
        rid = "gid")

# trajectory Type I
as_pgtraj(conn, 
        schema = "traj_t4",
        relocations_table = "example_data.reloc_t1", 
        pgtrajs = "small",
        animals = "small animal",
        relocations = "geom",
        rid = "gid")



