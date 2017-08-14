# ltraj to simple features

library(adehabitatLT)
# install.packages("hab", repos = "http://ase-research.org/R/", type = "source")
library(hab)
library(sf)
library(rpostgisLT)
library(lubridate)
library(dplyr)


# Prepare roe_sf.RData ----------------------------------------------------

# prepare a subset of roe_gps data as: 
# data frame -> ltraj -> sp -> sf
data("roe_gps_data")
gps_data <- rbind(roe_gps_data$GSM01511[c("gps_sensors_code","utc_date",
                                          "utc_time","latitude","longitude")],
                  roe_gps_data$GSM01512[c("gps_sensors_code","utc_date",
                                          "utc_time","latitude","longitude")])
gps_data$tstamp <- lubridate::dmy_hms(paste(gps_data[, "utc_date"],
                                            gps_data[, "utc_time"], sep = " "),
                                      tz = "UTC")
gps_data <- dplyr::rename(gps_data, x = longitude, y = latitude)
gps_data <- dplyr::select(gps_data, x, y, tstamp, gps_sensors_code)

roe_ltraj <- adehabitatLT::as.ltraj(gps_data, date = gps_data$tstamp,
                                    id = gps_data$gps_sensors_code)
roe_ltraj_reg <- adehabitatLT::redisltraj(roe_ltraj, 14400, type = "time")
roe_df <- hab::ltraj2sldf(na.omit(roe_ltraj_reg), by = "step")
roe_sf <- dplyr::arrange(sf::st_as_sf(roe_df), date)
sf::st_crs(roe_sf) = 4326
save(roe_sf, file = "./data/roe_sf.RData")

rm(roe_df, roe_gps_data, roe_ltraj, roe_ltraj_reg, gps_data)

# Prepare stork_2004_sf.rda ----------------------------------------------------

stork_2004_ltraj <- pgtraj2ltraj(conn, "2004", "stork_traj")
stork_2004_df <- hab::ltraj2sldf(na.omit(stork_2004_ltraj),
                                 by = "step")
stork_2004_sf <- 
    sf::st_as_sf(stork_2004_df) %>% 
    arrange(date) %>% 
    st_set_crs(4326)

save(stork_2004_sf, file = "./data/stork_2004_sf.rda")



# stork_2004 pgtraj to sf -------------------------------------------------

stork_2004_sf <- st_read_db(conn, table=c("stork_traj", "step_geometry_shiny_2004"),
                            stringsAsFactors=FALSE)
