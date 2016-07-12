## Basic ltraj
library(adehabitatLT)
data(ibexraw)
ibexraw                                 # No infolocs in ibexraw.
is.regular(ibexraw)
## FALSE

## Note that there is an issue with the time zone. In 'ibexraw', the
## time zone is not set:
attr(ld(ibexraw)$date, "tzone")
## This means that it is assumed to be UTC, and is thus converted to
## local time zone on display (EDT2EST for me):
head(ld(ibexraw)$date)                 # Note that the first timestamp
                                       # should be '2003-06-01
                                       # 00:00:56'
## We need to fix that upfront:
ibex <- ld(ibexraw)
attr(ibex$date, "tzone") <- "Europe/Paris"
ibex <- dl(ibex)

ltraj2pgtraj(ibex)                   # Default should be in schema
                                        # 'traj' and use ltraj name
                                        # ('ibexraw') as pgtraj name.
ibexTest <- pgtraj2ltraj("ibex")     # Default should look into
                                        # 'traj' schema.
identical(ibex, ibexTest)
## TRUE


## Missing relocations
refda <- strptime("2003-06-01 00:00", "%Y-%m-%d %H:%M",
    tz = "Europe/Paris")
(ibex <- setNA(ibex, refda, 4, units = "hour"))
ltraj2pgtraj(ibex, overwrite = TRUE)
ibexTest <- pgtraj2ltraj("ibex")
identical(ibex, ibexTest)
## TRUE


## Rounding timestamps
(ibex <- sett0(ibex, refda, 4, units = "hour"))
ibex.ref <- ibex                        # At this stage, 'ibex' is our
                                        # reference data
ltraj2pgtraj(ibex, overwrite = TRUE)
ibexTest <- pgtraj2ltraj("ibex")
identical(ibex, ibexTest)
## TRUE


## Interpolation

## 1. In space
summary(ld(ibex)$dist)
(ibex <- redisltraj(ibex, 400))         # Note that 'redisltraj'
                                        # creates an 'infolocs'
                                        # attribute, which we remove
                                        # for now:
ibex <- removeinfo(ibex)
ltraj2pgtraj(ibex, overwrite = TRUE)
ibexTest <- pgtraj2ltraj("ibex")
identical(ibex, ibexTest)
## TRUE

## 2. In time
ibex <- ibex.ref
(ibex <- redisltraj(na.omit(ibex), 14400, type = "time"))
ibex <- removeinfo(ibex)
ltraj2pgtraj(ibex, overwrite = TRUE)
ibexTest <- pgtraj2ltraj("ibex")
identical(ibex, ibexTest)
## TRUE


## Subset

## 1. Subset on given parameters
ibex <- ibex.ref
## We work on the data frame from the trajectory, which we subset, and
## then rebuild the ltraj without recomputing trajectory parameters;
## this is essentially what 'hab::subset' does.
## Note that the steps are not discontinuous any more.
ibex <- ld(ibex)
ibex <- droplevels(ibex[ibex$dist < 400 & !is.na(ibex$dist), ])
dlfast <- function(x) {
    trajnam <- c("x", "y", "date", "dx", "dy", "dist", "dt",
        "R2n", "abs.angle", "rel.angle")
    idd <- tapply(as.character(x$id), x$burst, unique)
    traj <- split(x[, names(x) %in% trajnam], x$burst)
    names(traj) <- NULL
    class(traj) <- c("ltraj", "list")
    attr(traj, "typeII") <- TRUE
    attr(traj, "regular") <- is.regular(traj)
    for (i in (1:length(traj))) {
        attr(traj[[i]], "id") <- as.character(idd[i])
        attr(traj[[i]], "burst") <- names(idd[i])
    }
    return(traj)
}
ibex <- dlfast(ibex)
head(ibex[[1]])
ltraj2pgtraj(ibex, overwrite = TRUE)
ibexTest <- pgtraj2ltraj("ibex")
identical(ibex, ibexTest)

## 2. Subsample on the temporal sequence
ibex <- ibex.ref
(ibex <- subsample(ibex, 14400*2))
ltraj2pgtraj(ibex, overwrite = TRUE)
ibexTest <- pgtraj2ltraj("ibex")
identical(ibex, ibexTest)


## Cut, bind bursts

## 1. Cut if there is a step greater than 3000 m
ibex <- ibex.ref
(ibex <- cutltraj(ibex, "dist > 3000"))
ltraj2pgtraj(ibex, overwrite = TRUE)
ibexTest <- pgtraj2ltraj("ibex")
identical(ibex, ibexTest)

## 2. Bind back by individual:
(ibex <- bindltraj(ibex))
ibex <- removeinfo(ibex)
ltraj2pgtraj(ibex, overwrite = TRUE)
ibexTest <- pgtraj2ltraj("ibex")
identical(ibex, ibexTest)


## Combine trajectories
ibex <- ibex.ref
ibex2 <- ibex
burst(ibex2) <- paste(burst(ibex2), "2", sep = "-")
(ibex <- c(ibex, ibex2)[order(id(c(ibex, ibex2)))])
ltraj2pgtraj(ibex, overwrite = TRUE)
ibexTest <- pgtraj2ltraj("ibex")
identical(ibex, ibexTest)
