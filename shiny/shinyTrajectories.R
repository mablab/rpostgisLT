# setwd("/rpostgisLT") # adjust to set working directory to "rpostgisLT"
source("./utility/connect_db.R")
source("./shiny/makeShinyView.R")
source("./shiny/pgtrajPlotter.R")
source("./shiny/ltrajPlotter.R")
data("roe_sf")

# Setup -------------------------------------------------------------------

# connect to db
# conn <- cs(pw="gsoc", host = "ufl")

# if you need to transfer pgtraj-es between databases, use the /utility/transfer_pgtraj.R script

# Ibex
schema <- "ibex_traj"
pgtraj <- "ibex"
d_start <- "2003-06-01" # first date
t_start <- "00:00:00" # first hour
tzone <- "Europe/Paris"
increment <- 4 # increment by 1 hours at a time
nr_increment <- 10 # increment 10x
interval <- 24 # hours of time window to load

# Storks
schema <- "stork_traj"
pgtraj <- "2004"
d_start <- "2004-06-01" # first date
t_start <- "00:00:00" # first hour
tzone <- "Europe/Amsterdam"
increment <- 24 # increment by 1 hours at a time
nr_increment <- 10 # increment 10x
interval <- 48 # hours of time window to load

# Need to create a new View in DB with EPSG:4326, because that's what Leaflet
# understands by default. Coordinate transformation can be expensive.
# Keep in mind that the current version of makeShinyView materializes the views

# However, the storks dataset is in EPSG:4326 already
makeShinyView(conn, schema, pgtraj)


# Run ---------------------------------------------------------------------

# pgtraj from database
pgtrajPlotter(conn,
              schema,
              pgtraj,
              d_start,
              t_start,
              tzone,
              increment,
              nr_increment,
              interval)

# ltraj-sf from R
ltrajPlotter(
    pgtraj_sf = roe_sf,
    d_start = "2005-10-22",
    t_start = "00:00:00",
    tzone = "UTC",
    increment = 4,
    interval = 48
)

