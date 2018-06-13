library(devtools)
library(roxygen2)
library(rmarkdown)
library(basr)
#library(rhub) # for testing on a variety of system configs

setwd("C:/David/git/rpostgisLT")
devtools::install()

# R build ignore files/folders
use_build_ignore("tests")
use_build_ignore("utility")
use_build_ignore("rpostgisLT.pdf")
use_build_ignore("cran-comments.md")


devtools::document()
spell_check()

run_examples()
check(cran=TRUE)
manual(overwrite=TRUE)
install()

build_win()

# vignette html
render("vignettes/data-model.Rmd",encoding="UTF-8")
render("vignettes/use-cases.Rmd",encoding="UTF-8")

# tests
source("tests/tests.R")

#test code coverage
# install.packages(c("covr","DT"))
library(covr)
x<-package_coverage(quiet = FALSE)
report(x)

#release to CRAN
devtools::release()


################
# example data
################

#package (example) data with rpostgis(LT)
# conn<-dbConnect("PostgreSQL",dbname = "demo", host = "basille-flrec", user = "", password = "")

roe_animals<-read.csv("utility/example_data_raw/data/animals/animals.csv",sep = ";", header = FALSE)
colnames(roe_animals)<-c("animals_id","animals_code","animal_name","sex","age_class_code","species_code")

roe_sensors<-read.csv("utility/example_data_raw/data/sensors/gps_sensors.csv",sep = ";", header = FALSE)
colnames(roe_sensors)<-c("gps_sensors_id","gps_sensors_code","purchase_date","frequency","vendor","model","sim")

roe_sensors_animals<-read.csv("utility/example_data_raw/data/sensors_animals/gps_sensors_animals.csv",sep=";",header=FALSE)
colnames(roe_sensors_animals)<-c("animals_id","gps_sensors_id","start_time","end_time","notes")
roe_sensors_animals$notes<-gsub("batettery","battery",roe_sensors_animals$notes,fixed = TRUE)

roe_sensors_animals_tables<-list(roe_animals,roe_sensors,roe_sensors_animals)
names(roe_sensors_animals_tables)<-c("animals","gps_sensors","gps_sensors_animals")
use_data(roe_sensors_animals_tables, overwrite = TRUE)

# flds<-dbListFields(conn,c("main","gps_data"))[2:47]
# flds
#  [1] "gps_sensors_code" "line_no"          "utc_date"         "utc_time"         "lmt_date"         "lmt_time"         "ecef_x"          
#  [8] "ecef_y"           "ecef_z"           "latitude"         "longitude"        "height"           "dop"              "nav"             
# [15] "validated"        "sats_used"        "ch01_sat_id"      "ch01_sat_cnr"     "ch02_sat_id"      "ch02_sat_cnr"     "ch03_sat_id"     
# [22] "ch03_sat_cnr"     "ch04_sat_id"      "ch04_sat_cnr"     "ch05_sat_id"      "ch05_sat_cnr"     "ch06_sat_id"      "ch06_sat_cnr"    
# [29] "ch07_sat_id"      "ch07_sat_cnr"     "ch08_sat_id"      "ch08_sat_cnr"     "ch09_sat_id"      "ch09_sat_cnr"     "ch10_sat_id"     
# [36] "ch10_sat_cnr"     "ch11_sat_id"      "ch11_sat_cnr"     "ch12_sat_id"      "ch12_sat_cnr"     "main_vol"         "bu_vol"          
# [43] "temp"             "easting"          "northing"         "remarks"   


g1<-read.csv("utility/example_data_raw/data/sensors_data/GSM01438.csv", sep = ";", stringsAsFactors = FALSE)
g1$UTC_DATE.D <- format(as.Date(g1$UTC_DATE.D, format = "%d/%m/%y"), "%d/%m/%Y") # 2-year utc to 4-year (to match others)
g1$LMT_DATE.D <- format(as.Date(g1$LMT_DATE.D, format = "%d/%m/%y"), "%d/%m/%Y") # 2-year utc to 4-year (to match others)
colnames(g1)<-flds
g2<-read.csv("utility/example_data_raw/data/sensors_data/GSM01508.csv", sep = ";", stringsAsFactors = FALSE)
colnames(g2)<-flds
g3<-read.csv("utility/example_data_raw/data/sensors_data/GSM01511.csv", sep = ";", stringsAsFactors = FALSE)
colnames(g3)<-flds
g4<-read.csv("utility/example_data_raw/data/sensors_data/GSM01512.csv", sep = ";", stringsAsFactors = FALSE)
colnames(g4)<-flds
g5<-read.csv("utility/example_data_raw/data/sensors_data/GSM02927.csv", sep = ";", stringsAsFactors = FALSE)
colnames(g5)<-flds

roe_gps_data<-list(g1,g2,g3,g4,g5)
names(roe_gps_data)<-c("GSM01438","GSM01508","GSM01511","GSM01512","GSM02927")
head(roe_gps_data)

use_data(roe_gps_data, overwrite = TRUE)

library(rgdal)
v1<-readOGR("utility/example_data_raw/data/env_data/vector","adm_boundaries")
v2<-readOGR("utility/example_data_raw/data/env_data/vector","meteo_stations")
v3<-readOGR("utility/example_data_raw/data/env_data/vector","roads")
v4<-readOGR("utility/example_data_raw/data/env_data/vector","study_area")

roe_vector_geom<-list(v1,v2,v3,v4)
names(roe_vector_geom)<-c("adm_boundaries","meteo_stations","roads","study_area")

use_data(roe_vector_geom, overwrite = TRUE)

library(raster)
r1<-raster("utility/example_data_raw/data/env_data/raster/corine06.tif", values = TRUE)
r1a<-raster(r1)
r1a<-setValues(r1a,getValues(r1))
r2<-raster("utility/example_data_raw/data/env_data/raster/srtm_dem.tif", values = TRUE)
r2a<-raster(r2)
r2a<-setValues(r2a,getValues(r2))

roe_raster<-list(r1a,r2a)
names(roe_raster)<-c("corine06","srtm_dem")
use_data(roe_raster, overwrite = TRUE)

##### end data