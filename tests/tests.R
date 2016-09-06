## Establish connection with rpostgisLT database
#source("./rpostgisLT/utility/utility_functions.R")
#cs() # creates globals conn and drv
library(rpostgisLT)
conn<-dbConnect(PostgreSQL(),dbname="rpostgis",host="localhost",user="postgres",password="pgis")

## Get test datasets
data(ibex)
data(ibexraw)
data(puechcirc)
data(albatross)
data(porpoise)

## Update ltraj with 'proj4string' attribute
ibex <- dl(ld(ibex))
ibexraw <- dl(ld(ibexraw))
puechcirc <- dl(ld(puechcirc))
albatross <- dl(ld(albatross))
porpoise <- dl(ld(porpoise))


## Minimal test
ib_min <- dl(ld(ibexraw[1])[1:10, ]) # note that step parameters are recomputed on purpose
ltraj2pgtraj(conn, schema = "traj_min", ltraj = ib_min, pgtraj = "ib_min")
ib_min_re <- pgtraj2ltraj(conn, schema = "traj_min", pgtraj = "ib_min")
all.equal(ib_min, ib_min_re)
identical(ib_min, ib_min_re)

dbDrop(conn, "traj_min", type = "schema", cascade = TRUE)
rm(ib_min_re, ib_min)

srs <- CRS("+init=epsg:3395")
ib_min_srs <- dl(ld(ibexraw[2])[1:10, ], proj4string = srs) # note that step 
# parameters are recomputed on purpose
ltraj2pgtraj(conn, schema = "traj_min", ltraj = ib_min_srs, pgtraj = "ib_min_3395")
ib_min_srs_re <- pgtraj2ltraj(conn, schema = "traj_min", pgtraj = "ib_min_3395")
all.equal(ib_min_srs, ib_min_srs_re)

dbDrop(conn, "traj_min", type = "schema", cascade = TRUE)
rm(ib_min_srs, ib_min_srs_re)


## Basic ltraj
ibexraw                                 # No infolocs in ibexraw.
is.regular(ibexraw)
## FALSE

ltraj2pgtraj(conn, ibex, overwrite = TRUE)                   # Default should be in schema
                                        # 'traj' and use ltraj name
                                        # ('ibex') as pgtraj name.
ibexTest <- pgtraj2ltraj(conn, pgtraj = "ibex")     # Default should look into
                                        # 'traj' schema.
all.equal(ibex, ibexTest)
# TRUE

dbDrop(conn, "traj", type = "schema", cascade = TRUE)
rm(ibexTest)

## More basic ltraj
srs2 <- CRS("+init=epsg:4326")
attr(ibexraw, 'proj4string') <- srs
attr(puechcirc, 'proj4string') <- srs2
attr(albatross, 'proj4string') <- srs
attr(porpoise, 'proj4string') <- srs2

ltraj2pgtraj(conn, ltraj = ibexraw, note = "test CRS on ibexraw", overwrite=TRUE)
ltraj2pgtraj(conn, ltraj = puechcirc, note = "test CRS on puechcirc",overwrite=TRUE)
ltraj2pgtraj(conn, ltraj = albatross, note = "test CRS on albatross",overwrite=TRUE)
ltraj2pgtraj(conn, ltraj = porpoise, note = "test CRS on porpoise",overwrite=TRUE)

ibexraw_re <- pgtraj2ltraj(conn, schema = 'traj', pgtraj = 'ibexraw')
puechcirc_re <- pgtraj2ltraj(conn, schema = 'traj', pgtraj = 'puechcirc')
albatross_re <- pgtraj2ltraj(conn, schema = 'traj', pgtraj = 'albatross') # there is an error with this
porpoise_re <- pgtraj2ltraj(conn, schema = 'traj', pgtraj = 'porpoise')

all.equal(ibexraw, ibexraw_re)
all.equal(puechcirc, puechcirc_re)
all.equal(albatross, albatross_re)
all.equal(porpoise, porpoise_re)

dbDrop(conn, "traj", type = "schema", cascade = TRUE)
rm(ibexraw_re, puechcirc_re, albatross_re, porpoise_re)

## Missing relocations
refda <- strptime("2003-06-01 00:00", "%Y-%m-%d %H:%M",
    tz = "Europe/Paris")
(ibex <- setNA(ibex, refda, 4, units = "hour"))
ltraj2pgtraj(conn, ibex, overwrite = TRUE)
ibexTest <- pgtraj2ltraj(conn, pgtraj = "ibex")
all.equal(ibex, ibexTest)
# TRUE
dbDrop(conn, "traj", type = "schema", cascade = TRUE)
rm(ibexTest)


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
summary(ld(ibex)$dist)
(ibex <- redisltraj(ibex, 400,type="space"))         # Note that 'redisltraj'
                                        # creates an 'infolocs'
                                        # attribute, which we remove
                                        # for now:
                                        # this function is adding an extra column - "rel.ang"
ibex <- removeinfo(ibex)
ibex[[1]]$rel.ang<-NULL

ltraj2pgtraj(conn, ibex, overwrite = TRUE)
ibexTest <- pgtraj2ltraj(conn, pgtraj = "ibex")
all.equal(ibex, ibexTest) # not TRUE

for (i in 1:10) {
 print(i)
  print(all.equal(ibex[[1]][,i],ibexTest[[1]][,i]))
}
#dates attributes are switched around, but all other columns are equal. Should not be a problem(?)
#> attr(ibex[[1]]$date,"class")
#[1] "POSIXt"  "POSIXct"
#> attr(ibexTest[[1]]$date,"class")
#[1] "POSIXct" "POSIXt" 
all.equal(ibex[[1]][,3],ibexTest[[1]][,3])

head(ibex[[1]])
head(ibexTest[[1]])
attributes(ibex[[1]]$date)
attributes(ibexTest[[1]]$date)

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
## Note that the steps are not continuous any more.
ibex <- ld(ibex)
ibex <- droplevels(ibex[ibex$dist < 400 & !is.na(ibex$dist), ])
ibex <- dl(ibex)
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
attr(ibex,"proj4string") <- CRS() # proj4string attributes needs to be added
ltraj2pgtraj(conn, ibex, overwrite = TRUE)
ibexTest <- pgtraj2ltraj(conn, pgtraj = "ibex")
all.equal(ibex, ibexTest)


#############################################################################
## Test database import
conn<-dbConnect(PostgreSQL(),host="basille-flrec",user="rpostgis",password="gsoc")

# all variables stored with the raw data
as_pgtraj(conn, 
        schema = "traj_db_t1",
        relocations_table = c("example_data","relocations_plus"),
        pgtrajs = "id",
        animals = "animal",
        bursts = "burst",
        relocations = "geom",
        timestamps = "time",
        rid = "gid")

continental <- pgtraj2ltraj(conn, "traj_db_t1", "continental")
large <- pgtraj2ltraj(conn, "traj_db_t1", "large")
large2 <- pgtraj2ltraj(conn, "traj_db_t1", "large2")
medium <- pgtraj2ltraj(conn, "traj_db_t1", "medium")
small <- pgtraj2ltraj(conn, "traj_db_t1", "small")

# relocations are provided as X,Y coordinates
as_pgtraj(conn, 
        schema = "traj_t2",
        relocations_table = c("example_data","relocations_plus"),
        pgtrajs = "id",
        animals = "animal",
        bursts = "burst",
        relocations = c("x", "y"),
        timestamps = "time",
        rid = "gid")

medium <- pgtraj2ltraj(conn, "traj_t2", "medium")

# variables provided manually
as_pgtraj(conn, 
        schema = "traj_t3",
        relocations_table = c("example_data","reloc_medium"), 
        pgtrajs = "medium",
        animals = "sea turtle",
        relocations = "geom",
        timestamps = "time",
        rid = "gid")

# Clean up
dbDrop(conn, "traj", type = "schema", cascade = TRUE)
dbDrop(conn, "traj_db_t1", type = "schema", cascade = TRUE)
dbDrop(conn, "traj_t2", type = "schema", cascade = TRUE)
dbDrop(conn, "traj_t3", type = "schema", cascade = TRUE)
rm(albatross, continental, ibex, ibex2, ibexraw, ibex.ref, ibexTest, large,
        large2, medium, porpoise, puechcirc, small, srs, srs2)

#############################################################################
## Test parameter computation

data(ibex)
data(albatross)
data(porpoise)
#recompute parameters
ibex <- dl(ld(ibex))
albatross <- dl(ld(albatross))
porpoise <- dl(ld(porpoise))


ibex_dl <- ld(ibex)
dbDrop(conn, name = c("example_data", "ibex"), type = "table", ifexists = TRUE)
pgInsert(conn, name = c("example_data", "ibex"), data.obj = ibex_dl, new.id = "gid")

as_pgtraj(conn, 
        schema = "traj",
        relocations_table = c("example_data","ibex"),
        pgtraj = "ibex",
        animals = "id",
        bursts = "burst",
        relocations = c("x", "y"),
        timestamps = "date",
        rid = "gid")
ibex_re <- pgtraj2ltraj(conn, "traj", "ibex")
all.equal(ibex, ibex_re)
# gives warning of inconsistent time zone attribute but that is expected

albatross_dl <- ld(albatross)
dbDrop(conn, name = c("example_data", "albatross"), type = "table", ifexists = TRUE)
pgInsert(conn, name = c("example_data", "albatross"), data.obj = albatross_dl, new.id = "gid")

as_pgtraj(conn, 
        schema = "traj",
        relocations_table = c("example_data","albatross"),
        pgtraj = "albatross",
        animals = "id",
        bursts = "burst",
        relocations = c("x", "y"),
        timestamps = "date",
        rid = "gid")
albatross_re <- pgtraj2ltraj(conn, "traj", "albatross")
all.equal(albatross, albatross_re)
# gives warning of inconsistent time zone attribute but that is expected
# furthermore gives a high number of 'Mean absoloute difference'
# on the 'date' column 'Component 3', which is
# also expected, because the original albatross is in UTC and albatross_re 
# is in local time zone, thus the 'Mean absoloute difference' is the time
# differece between the two time zones

# Clean up
dbDrop(conn, "traj", type = "schema", cascade = TRUE)
rm(ibex, ibex_re, albatross, albatross_re, albatross_dl, ibex_dl, refda,
        porpoise)

