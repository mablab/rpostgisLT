## Establish connection with rpostgisLT database
source("./rpostgisLT/tests/utility_functions.R")
cs() # creates globals conn and drv


data(ibexraw)
data(ibex)
data(puechcirc)
data(albatross)
data(porpoise)


## Minimal test
ib_min <- dl(ld(ibexraw[1])[1:10, ]) # note that step parameters are recomputed on purpose
ltraj2pgtraj(conn, schema = "traj_min", ltraj = ib_min, pgtraj = "ib_min")
ib_min_re <- pgtraj2ltraj(conn, schema = "traj_min", pgtraj = "ib_min")
all.equal(ib_min, ib_min_re)

srs <- CRS("+init=epsg:3395")
ib_min_srs <- dl(ld(ibexraw[2])[1:10, ], proj4string = srs) # note that step 
# parameters are recomputed on purpose
ltraj2pgtraj(conn, schema = "traj_min", ltraj = ib_min_srs, pgtraj = "ib_min_3395")
ib_min_srs_re <- pgtraj2ltraj(conn, schema = "traj_min", pgtraj = "ib_min_3395")
all.equal(ib_min_srs, ib_min_srs_re)


## Basic ltraj
ibexraw                                 # No infolocs in ibexraw.
is.regular(ibexraw)
## FALSE

ltraj2pgtraj(conn, ibex)                   # Default should be in schema
                                        # 'traj' and use ltraj name
                                        # ('ibex') as pgtraj name.
ibexTest <- pgtraj2ltraj(conn, pgtraj = "ibex")     # Default should look into
                                        # 'traj' schema.
all.equal(ibex, ibexTest)
## TRUE

## More basic ltraj
srs2 <- CRS("+init=epsg:4326")
attr(ibexraw, 'proj4string') <- srs
attr(puechcirc, 'proj4string') <- srs2
attr(albatross, 'proj4string') <- srs
attr(porpoise, 'proj4string') <- srs2

ltraj2pgtraj(conn, ltraj = ibexraw, comment = "test CRS on ibexraw")
ltraj2pgtraj(conn, ltraj = puechcirc, comment = "test CRS on puechcirc")
ltraj2pgtraj(conn, ltraj = albatross, comment = "test CRS on albatross")
ltraj2pgtraj(conn, ltraj = porpoise, comment = "test CRS on porpoise")

ibexraw_re <- pgtraj2ltraj(conn, schema = 'traj', pgtraj = 'ibexraw')
puechcirc_re <- pgtraj2ltraj(conn, schema = 'traj', pgtraj = 'puechcirc')
albatross_re <- pgtraj2ltraj(conn, schema = 'traj', pgtraj = 'albatross')
#Error in `row.names<-.data.frame`(`*tmp*`, value = value) : 
#        duplicate 'row.names' are not allowed
#In addition: Warning message:
#        non-unique value when setting 'row.names': ‘65’ 
porpoise_re <- pgtraj2ltraj(conn, schema = 'traj', pgtraj = 'porpoise')

all.equal(ibexraw, ibexraw_re)
all.equal(puechcirc, puechcirc_re)
#all.equal(albatross, albatross_re)
all.equal(porpoise, porpoise_re)



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
