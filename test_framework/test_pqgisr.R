library(rPython)
library(pathological)
library(RQGIS)
library(rgdal)
library(devtools)
library(wkb)
# download repo from https://gitlab.com/b-rowlingson/pqgisr.git
# then load it:
devtools::load_all("../pqgisr")
library(pqgisr)

# DB connection for OGR
connect_db_ogr <- function(host = "local", pw, pgtraj) {
    if (host == "local") {
        DB <- "rpostgisLT"
        HOST <- "localhost"
        USER <- "bdukai"
        PSSWD <- pw
        dsn <- paste0("PG:dbname=", DB, " host=", HOST, " user=", USER,
                      " password=", PSSWD," schemas=roe_traj 
                      tables=step_geometry_", pgtraj)
    } else if (host == "ufl") {
        conn <<- dbConnect(drv, user="rpostgis", password=pw, dbname="rpostgis",
                           host="basille-flrec.ad.ufl.edu")
        message(paste("Connection to host",host,"established successfully"))
        DB <- "rpostgis"
        HOST <- "basille-flrec.ad.ufl.edu"
        USER <- "rpostgis"
        PSSWD <- pw
        dsn <- paste0("PG:dbname=", DB, " host=", HOST, " user=", USER,
                      " password=", PSSWD," schemas=roe_traj 
                      tables=step_geometry_", pgtraj)
    }
    print(dsn)
    return(dsn)
}

dsn <- connect_db_ogr(host = "ufl", pw="gsoc", pgtraj = "bondone")
# test connection
ogrListLayers(dsn)

# Read directly from database
init_qgis()
bondone <- add_ogr_layer(dsn)
set_extent(bondone)
qgis
#remove_layer(bondone)
end_qgis()

# Read from sp object
# I didn't copy the whole DB connection functions again, so please reuse the 
# connection object, then:
schema <- "roe_traj"
pgtraj <- "bondone"
view <- paste0("step_geometry_", pgtraj)
SQL <- paste0("select step_id, step_geom, relocation_time
                  from ", schema, ".", view,
              " where step_geom is not null")
x <- pgGetGeom(conn, geom = "step_geom", query = SQL)

init_qgis()
bondone <- add_sp_layer(x, "bondone")
set_extent(bondone)
qgis
#remove_layer(bondone)
end_qgis()

