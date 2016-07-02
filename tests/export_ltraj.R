# 
# Author: bdukai
###############################################################################

library(RPostgreSQL)
library(adehabitatLT)


detach("package:adehabitatLT", unload=TRUE)

# get sample ltraj
data(ibex)
ibex_df <- ld(ibex)

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


# ltraj2pgtraj
source("./R/ld_opt.R")

ibex_df2 <- ld_opt(ibex) # convert to data frame with Mathieu's function
ibex2 <- dl_opt(ibex_df2)
identical(ibex, ibex2)
