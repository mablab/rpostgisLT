# setwd("/rpostgisLT") # adjust to set working directory to "rpostgisLT"
library(rpostgisLT)
source("./utility/connect_db.R")

# Setup -------------------------------------------------------------------

# connect to db
# conn <- cs(pw="gsoc", host = "ufl")

# if you need to transfer pgtraj-es between databases, use the /utility/transfer_pgtraj.R script

# Ibex
schema <- "ibex_traj"
pgtraj <- "ibex"

schema <- "ibex_traj_materialized_bursts"
pgtraj <- "ibex_int_space"

# Storks
schema <- "stork_traj"
pgtraj <- "2004"

# Roe deer
schema <- "roe_traj"
pgtraj <- "bondone"
pgtraj <- "rendena"


# Need to create a new View in DB with EPSG:4326, because that's what Leaflet
# understands by default. Coordinate transformation can be expensive.
# Keep in mind that the current version of makeShinyView materializes the views

# However, the storks dataset is in EPSG:4326 already
rpostgisLT:::createShinyStepsView(conn, schema, pgtraj)
rpostgisLT:::createShinyBurstsView(conn, schema)

layers_vector <- list(c("example_data", "county_subdiv"), c("example_data", "test_points"))
layers_params_vector <- list(test_points=list(color = "red", stroke = FALSE, fillOpacity = 0.5),
                      county_subdiv=list(color = "grey", fillOpacity = 0.2))

# Run ---------------------------------------------------------------------

explorePgtraj(conn,
              schema,
              pgtraj)

# background layers
conn <- do.call(cs, args)
explorePgtraj(conn, schema, pgtraj)
dbDisconnect(conn)

conn <- do.call(cs, args)
explorePgtraj(conn, schema, pgtraj, layers_vector)
dbDisconnect(conn)


explorePgtraj(
    conn_data,
    "ibex_traj_materialized_bursts",
    "ibex_int_space",
    layers_vector = list(c("example_data", "test_points_mixed"))
)

explorePgtraj(
    conn_data,
    "ibex_traj_materialized_bursts",
    "ibex_int_space",
    layers_vector = list(c("example_data", "test_points_multi_3395"))
)

explorePgtraj(
    conn_data,
    "ibex_traj_materialized_bursts",
    "ibex_int_space",
    layers_vector = list(c("example_data", "test_polygons"))
)

explorePgtraj(
    conn,
    "stork_traj",
    "2004",
    layers_vector = list(c("example_data", "test_points"))
)

conn <- do.call(cs, args)
explorePgtraj(conn, schema, pgtraj, layers_vector, layers_params_vector)
dbDisconnect(conn)

ras <- rgdal::readGDAL("./temp_data/florida_dem_county099.tif")
ras2 <- raster::raster(ras, 1)
ras2_leaflet <- leaflet::projectRasterForLeaflet(ras2)
conn <- do.call(cs, args)
explorePgtraj(conn, schema, pgtraj, layers_vector, layers_params_vector,
              layer_raster=ras2_leaflet)
dbDisconnect(conn)

