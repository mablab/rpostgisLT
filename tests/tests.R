## Establish connection with rpostgisLT database
#source("C:/David/git/rpostgisLT/utility/utility_functions.R")
#cs("") # creates globals conn and drv
library(rpostgisLT)

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

## Create Type I ltraj
ibexraw_I <- typeII2typeI(ibexraw)
albatross_I <- typeII2typeI(albatross)
porpoise_I <- typeII2typeI(porpoise)


## Minimal test
ib_min <- dl(ld(ibexraw[1])[1:10, ]) # note that step parameters are recomputed on purpose
ltraj2pgtraj(conn, schema = "traj_min", ltraj = ib_min, pgtraj = "ib_min")
ib_min_re <- pgtraj2ltraj(conn, schema = "traj_min", pgtraj = "ib_min")
all.equal(ib_min, ib_min_re)
Sys.sleep(2)
identical(ib_min, ib_min_re)

dbDrop(conn, "traj_min", type = "schema", cascade = TRUE)
rm(ib_min_re, ib_min)

srs <- CRS("+init=epsg:3395")
ib_min_srs <- dl(ld(ibexraw[2])[1:10, ], proj4string = srs) # note that step 
# parameters are recomputed on purpose
ltraj2pgtraj(conn, schema = "traj_min", ltraj = ib_min_srs, pgtraj = "ib_min_3395")
ib_min_srs_re <- pgtraj2ltraj(conn, schema = "traj_min", pgtraj = "ib_min_3395")
all.equal(ib_min_srs, ib_min_srs_re)
Sys.sleep(2)

dbDrop(conn, "traj_min", type = "schema", cascade = TRUE)
rm(ib_min_srs, ib_min_srs_re)


## Basic ltraj
ibexraw                                 # No infolocs in ibexraw.
is.regular(ibexraw)
## FALSE

ltraj2pgtraj(conn, ibex, overwrite = TRUE) # Default should be in schema
                                           # 'traj' and use ltraj name
                                           # ('ibex') as pgtraj name.
ibexTest <- pgtraj2ltraj(conn, pgtraj = "ibex")     # Default should look into
                                                    # 'traj' schema.
all.equal(ibex, ibexTest)
Sys.sleep(2)

dbDrop(conn, "traj", type = "schema", cascade = TRUE)
rm(ibexTest)

## More basic ltraj
srs2 <- CRS("+init=epsg:4326")
attr(ibexraw, 'proj4string') <- srs
attr(puechcirc, 'proj4string') <- srs2
attr(albatross, 'proj4string') <- srs
attr(porpoise, 'proj4string') <- srs2
# Type I
attr(porpoise_I, 'proj4string') <- srs2
attr(albatross_I, 'proj4string') <- srs
attr(ibexraw_I, 'proj4string') <- srs

ltraj2pgtraj(conn, ltraj = ibexraw, note = "test CRS on ibexraw", overwrite=TRUE)
ltraj2pgtraj(conn, ltraj = puechcirc, note = "test CRS on puechcirc",overwrite=TRUE)
ltraj2pgtraj(conn, ltraj = albatross, note = "test CRS on albatross",overwrite=TRUE)
ltraj2pgtraj(conn, ltraj = porpoise, note = "test CRS on porpoise",overwrite=TRUE)
# Type I
ltraj2pgtraj(conn, schema = "type_I", ltraj = porpoise_I, note = "arbitrary CRS")
ltraj2pgtraj(conn, schema = "type_I", ltraj = albatross_I, note = "arbitrary CRS")
ltraj2pgtraj(conn, schema = "type_I", ltraj = ibexraw_I, note = "arbitrary CRS")

ibexraw_re <- pgtraj2ltraj(conn, schema = 'traj', pgtraj = 'ibexraw')
puechcirc_re <- pgtraj2ltraj(conn, schema = 'traj', pgtraj = 'puechcirc')
albatross_re <- pgtraj2ltraj(conn, schema = 'traj', pgtraj = 'albatross')
porpoise_re <- pgtraj2ltraj(conn, schema = 'traj', pgtraj = 'porpoise')
# Type I
porpoise_I_re <- pgtraj2ltraj(conn, schema = "type_I", pgtraj = "porpoise_I")
albatross_I_re <- pgtraj2ltraj(conn, schema = "type_I", pgtraj = "albatross_I")
ibexraw_I_re <- pgtraj2ltraj(conn, schema = "type_I", pgtraj = "ibexraw_I")

all.equal(ibexraw, ibexraw_re)
all.equal(puechcirc, puechcirc_re)
all.equal(albatross, albatross_re)
all.equal(porpoise, porpoise_re)
# Type I
all.equal(ibexraw_I, ibexraw_I_re)
all.equal(porpoise_I, porpoise_I_re)
all.equal(albatross_I, albatross_I_re)
Sys.sleep(2)


# Clean up
dbDrop(conn, "traj", type = "schema", cascade = TRUE)
dbDrop(conn, "type_I", type = "schema", cascade = TRUE)
rm(ibexraw_re, puechcirc_re, albatross_re, porpoise_re, ibexraw_I_re,
        albatross_I_re, porpoise_I_re)

## Missing relocations
refda <- strptime("2003-06-01 00:00", "%Y-%m-%d %H:%M",
    tz = "Europe/Paris")
(ibex <- setNA(ibex, refda, 4, units = "hour"))
ltraj2pgtraj(conn, ibex, overwrite = TRUE)
ibexTest <- pgtraj2ltraj(conn, pgtraj = "ibex")
all.equal(ibex, ibexTest)
Sys.sleep(2)

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
Sys.sleep(2)


## Interpolation


## 1. In space
summary(ld(ibex)$dist)
(ibex <- redisltraj(ibex, 400,type="space"))         # Note that 'redisltraj'
                                        # creates an 'infolocs'
                                        # attribute, which is 
                                        # a factor (but should be probably be a character)
#ibex <- removeinfo(ibex)

ltraj2pgtraj(conn, ibex, overwrite = TRUE, infolocs = TRUE)
ibexTest <- pgtraj2ltraj(conn, pgtraj = "ibex")
all.equal(ibex, ibexTest) # not TRUE... date rounding
Sys.sleep(2)

#time rounding causing all.equal == FALSE
#ibexTest[[1]]$date == ibex[[1]]$date
all.equal(as.integer(ibex[[1]]$date),as.integer(ibexTest[[1]]$date))

all.equal(ibex[[1]][,3],ibexTest[[1]][,3])
Sys.sleep(2)

## 2. In time
ibex <- ibex.ref
(ibex <- redisltraj(na.omit(ibex), 14400, type = "time"))
#ibex <- removeinfo(ibex)
ltraj2pgtraj(conn, ibex, overwrite = TRUE, infolocs = TRUE)
ibexTest <- pgtraj2ltraj(conn, pgtraj = "ibex")
all.equal(ibex, ibexTest)
## TRUE
Sys.sleep(2)


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
Sys.sleep(2)

## 2. Subsample on the temporal sequence
ibex <- ibex.ref
(ibex <- subsample(ibex, 14400*2))
ltraj2pgtraj(conn, ibex, overwrite = TRUE)
ibexTest <- pgtraj2ltraj(conn, pgtraj = "ibex")
all.equal(ibex, ibexTest)
Sys.sleep(2)


## Cut, bind bursts

## 1. Cut if there is a step greater than 3000 m
ibex <- ibex.ref
(ibex <- cutltraj(ibex, "dist > 3000"))
ltraj2pgtraj(conn, ibex, overwrite = TRUE)
ibexTest <- pgtraj2ltraj(conn, pgtraj = "ibex")
all.equal(ibex, ibexTest)
Sys.sleep(2)

## 2. Bind back by individual:
(ibex <- bindltraj(ibex))
ibex <- removeinfo(ibex)
ltraj2pgtraj(conn, ibex, overwrite = TRUE)
ibexTest <- pgtraj2ltraj(conn, pgtraj = "ibex")
all.equal(ibex, ibexTest)
Sys.sleep(2)


## Combine trajectories
ibex <- ibex.ref
ibex2 <- ibex
burst(ibex2) <- paste(burst(ibex2), "2", sep = "-")
(ibex <- c(ibex, ibex2)[order(id(c(ibex, ibex2)))])
attr(ibex,"proj4string") <- CRS() # proj4string attributes needs to be added
ltraj2pgtraj(conn, ibex, overwrite = TRUE)
ibexTest <- pgtraj2ltraj(conn, pgtraj = "ibex")
all.equal(ibex, ibexTest)
Sys.sleep(2)

#############################################################################
## Test database import

# all variables stored with the raw data
# infolocs in same relocations_table

as_pgtraj(conn, 
        schema = "traj_db_t1",
        relocations_table = c("example_data","relocations_plus"),
        pgtrajs = "id",
        animals = "animal",
        bursts = "burst",
        relocations = "geom",
        timestamps = "time",
        rids = "gid",
        clauses = "where id = 'continental'",
        info_cols = c("info_day","dummy")
        )

dbDrop(conn,name = "traj_db_t1",type = "schema",cascade = TRUE)

# infolocs in other table
as_pgtraj(conn, 
        schema = "traj_db_t1",
        relocations_table = c("example_data","relocations_plus"),
        pgtrajs = "id",
        animals = "animal",
        bursts = "burst",
        relocations = "geom",
        timestamps = "time",
        rid = "gid",
        #clauses = "where id = 'continental'",
        info_cols = c("info_day","dummy"),
        info_table = c("infoloc_test"),
        info_rids = "gid"
        )

# Type I trajectories
as_pgtraj(conn,
        schema = "traj_db_t2",
        relocations_table = c("example_data","reloc_t1"),
        pgtrajs = "type_1",
        animals = "bunny",
        relocations = "geom",
        rid = "gid"
        )

# mix Type I and Type II in the same schema
as_pgtraj(conn, 
        schema = "traj_db_t2",
        relocations_table = c("example_data","relocations_plus"),
        pgtrajs = "id",
        animals = "animal",
        bursts = "burst",
        relocations = c("x", "y"),
        clauses = "where id = 'medium'",
        timestamps = "time",
        rid = "gid")


continental <- pgtraj2ltraj(conn, "continental" ,"traj_db_t1")
large <- pgtraj2ltraj(conn, "large" , "traj_db_t1")
medium <- pgtraj2ltraj(conn, "medium" , "traj_db_t1")
small <- pgtraj2ltraj(conn, "small" , "traj_db_t1")
type_1 <- pgtraj2ltraj(conn, "type_1", "traj_db_t2")

ltraj2pgtraj(conn,continental,"traj_db_t1",overwrite = TRUE, infolocs = TRUE)
ltraj2pgtraj(conn,large,"traj_db_t1",overwrite = TRUE, infolocs = TRUE)
ltraj2pgtraj(conn,medium,"traj_db_t1",overwrite = TRUE, infolocs = TRUE)
ltraj2pgtraj(conn,small,"traj_db_t1",overwrite = TRUE, infolocs = TRUE)
ltraj2pgtraj(conn, type_1, "traj_db_t2", overwrite = TRUE)
ltraj2pgtraj(conn, ltraj = type_1, schema = "traj_db_t2", pgtraj = "type_1_re")

continental2 <- pgtraj2ltraj(conn,  "continental" ,"traj_db_t1")
large2 <- pgtraj2ltraj(conn, "large" , "traj_db_t1")
medium2 <- pgtraj2ltraj(conn, "medium" , "traj_db_t1")
small2 <- pgtraj2ltraj(conn, "small" , "traj_db_t1")
type_1_2 <- pgtraj2ltraj(conn, "type_1", "traj_db_t2")

all.equal(continental,continental2)
all.equal(large,large2)
all.equal(medium,medium2)
all.equal(small,small2)
all.equal(type_1, type_1_2)
Sys.sleep(2)

# relocations are provided as X,Y coordinates
as_pgtraj(conn, 
        schema = "traj_t2",
        relocations_table = c("example_data","relocations_plus"),
        pgtrajs = "id",
        animals = "animal",
        bursts = "burst",
        relocations = c("x", "y"),
        clauses = "where id = 'medium'",
        timestamps = "time",
        rid = "gid")

medium <- pgtraj2ltraj(conn, "medium", "traj_t2")

# variables provided manually
as_pgtraj(conn, 
        schema = "traj_t3",
        relocations_table = c("example_data","reloc_medium"), 
        pgtrajs = "medium",
        animals = "sea turtle",
        relocations = "geom",
        timestamps = "time",
        rid = "gid")

pgtraj2ltraj(conn,"medium","traj_t3")

# Clean up
dbDrop(conn, "traj", type = "schema", cascade = TRUE)
dbDrop(conn, "traj_db_t1", type = "schema", cascade = TRUE)
dbDrop(conn, "traj_db_t2", type = "schema", cascade = TRUE)
dbDrop(conn, "traj_t2", type = "schema", cascade = TRUE)
dbDrop(conn, "traj_t3", type = "schema", cascade = TRUE)
rm(albatross, continental, ibex, ibex2, ibexraw, ibex.ref, ibexTest, large,
        large2, medium, porpoise, puechcirc, small, srs, srs2, type_1, type_1_2,
        ibexraw_I, albatross_I, porpoise_I)

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
        pgtrajs = "ibex",
        animals = "id",
        bursts = "burst",
        relocations = c("x", "y"),
        timestamps = "date",
        rids = "gid")
ibex_re <- pgtraj2ltraj(conn, "ibex")
all.equal(ibex, ibex_re)
# gives warning of inconsistent time zone attribute but that is expected
Sys.sleep(2)

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
albatross_re <- pgtraj2ltraj(conn, "albatross")
all.equal(albatross, albatross_re)

Sys.sleep(2)

# gives warning of inconsistent time zone attribute but that is expected
# furthermore gives a high number of 'Mean absoloute difference'
# on the 'date' column 'Component 3', which is
# also expected, because the original albatross is in UTC and albatross_re 
# is in local time zone, thus the 'Mean absoloute difference' is the time
# differece between the two time zones


# infolocs ltraj2pgtraj
## example of an object with an attribute "infolocs"
data(capreochiz)
head(capreochiz)
## Create an object of class "ltraj"
cap <- as.ltraj(xy = capreochiz[,c("x","y")], date = capreochiz$date,
                id = "Roe.Deer", typeII = TRUE,
                infolocs = capreochiz[,4:8])
#split it
cap <- cutltraj(cap, "dist > 100")
#add dummy column manually to one burst
#infolocs(cap)[[1]]$dummy<-1
# dumb row names
#row.names(cap[[1]])<-11111:(11111+length(cap[[1]]$x)-1)

ltraj2pgtraj(conn,cap,infolocs = TRUE, overwrite=TRUE)

cap2<-pgtraj2ltraj(conn,"cap")
all.equal(cap,cap2)
# differences due to "dummy" being included in every burst infolocs, not just the first (unless not created above)
Sys.sleep(2)


## additional infolocs test with other column types
data(capreochiz)
head(capreochiz)
str(capreochiz)

## POSIXt
library(lubridate)
attributes(capreochiz$date)

## Messing with timezone: timez
capreochiz$timez <- with_tz(capreochiz$date, tz = "America/Chicago")
attributes(capreochiz$timez)

## Messing with data class (and time zones!): posixlt
capreochiz$posixlt <- as.POSIXlt(capreochiz$date)
attributes(capreochiz$posixlt)

# this should be not allowed (step_id column is reserved for DB join)
# capreochiz$step_id <- 1


## Factors
library(forcats)

## Factor with an empty level: Status
table(capreochiz$Status)
levels(capreochiz$Status)               # Note that the levels have
                                        # extra space (both at the
                                        # beginning and end)

## Factor with NAs: facNA
capreochiz$facNA <- fct_recode(capreochiz$Status, NULL = " 3DF  ")
table(capreochiz$facNA, useNA = c("ifany"))

## Ordered factor (and empty level!): facOrd
capreochiz$facOrd <- fct_recode(capreochiz$Status, OK = " 2D   ", bad = " 2DDi ", good = " 3DDif", NULL = " 3DF  ", unknown = " Aqu  ")
capreochiz$facOrd <- factor(capreochiz$facOrd, levels = c("unknown", "bad", "OK", "good"), ordered = TRUE)
table(capreochiz$facOrd)
class(capreochiz$facOrd)                # Note that it's "ordered"
                                        # first, not "factor"!

## Build the ltraj
cap <- as.ltraj(xy = capreochiz[, c("x", "y")], date = capreochiz$date,
                id = "Roe.Deer", typeII = TRUE, infolocs = capreochiz[, 3:ncol(capreochiz)])
                                        # Note that I keep "date" in
                                        # infolocs as a reference; but
                                        # that also makes two date
                                        # fields! (ld converts the
                                        # second one to 'date.1')
str(infolocs(cap))

cap.test<-cap
# send to database
ltraj2pgtraj(conn,cap.test,infolocs = TRUE, overwrite=TRUE)
cap2<-pgtraj2ltraj(conn,pgtraj="cap.test")

all.equal(cap.test,cap2) 
Sys.sleep(2)

# Clean up
dbDrop(conn, "traj", type = "schema", cascade = TRUE)
rm(ibex, capreochiz, ibex_re, albatross, albatross_re, albatross_dl, ibex_dl, refda,
        porpoise, cap, cap.test, cap2, continental2, medium2, small2)
