# TODO: Add comment
# 
# Author: bdukai
###############################################################################

library(RPostgreSQL)
library(adehabitatLT)

drv <- dbDriver("PostgreSQL")
conn <- dbConnect(drv, user="rpostgis", password="gsoc", dbname="rpostgis",
        host="basille-flrec.ad.ufl.edu")

# get sample ltraj
data(ibex)
ibex_df <- ld(ibex) # convert to data frame

# create table with matching field names
tbl <- paste0('CREATE TABLE example_data.ibex (x integer, y integer, 
date timestamptz, dx integer, dy integer, dist numeric, dt numeric, ',
'"r2n"',' numeric,', '"abs_angle"',' numeric, ','"rel_angle"',
' numeric, id text, burst text)')
dbSendQuery(conn, tbl)

records <- pgInsertize(df=ibex_df)
pgInsert(conn,c("example_data","ibex"),pgi=records)

data(porpoise)
(porpoise2 <- redisltraj(na.omit(porpoise[1:3]), 86400, type="time"))
