# setwd("/rpostgisLT") # adjust to set working directory to "rpostgisLT"
source("./utility/connect_db.R")
source("./shiny/makeShinyView.R")
source("./shiny/trajPlotter.R")


# Setup -------------------------------------------------------------------

# connect to db
# conn <- cs(pw="gsoc", host = "ufl")

# if you need to transfer pgtraj-es between databases, use the /utility/transfer_pgtraj.R script

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
# makeShinyView(conn, schema, pgtraj)


# Run ---------------------------------------------------------------------

trajPlotter(conn, schema, pgtraj, d_start, t_start, tzone, increment,
            nr_increment, interval)