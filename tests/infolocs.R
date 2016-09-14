library(devtools)
#install_github('mablab/rpostgis')
library(jsonlite)
library(rpostgisLT)

conn<-dbConnect("PostgreSQL",dbname="rpostgis",user="postgres",password="pgis",host="localhost")

#dbSendQuery(conn,"DROP TABLE jsontest;")
#dbSendQuery(conn,"CREATE TABLE jsontest (id int primary key, burst int, jsonval jsonb);")

## example of an object with an attribute "infolocs"
data(capreochiz)
cap.alt<-capreochiz
#cap.alt$dist<-10
#cap.alt$Status<-droplevels(cap.alt$Status)  # if not, unused levels cause all.equal != TRUE
cap.alt$date2<-cap.alt$date
cap.alt$date3<-cap.alt$date
cap.alt$stat2<-cap.alt$Status

#mess with tz
attr(cap.alt$date3,"tzone")<-"America/New_York"
#attr(cap.alt$date2,"tzone")<-"bla" #this still causes errors
attr(cap.alt$date2,"tzone")<-"Europe/Paris" #this works


#head(capreochiz)
## Create an object of class "ltraj"
#cap <- as.ltraj(xy = cap.alt[,c("x","y")], date = cap.alt$date,
#                id = "Roe.Deer", typeII = TRUE,
#                infolocs = cap.alt[,4:8])


## Create an object of class "ltraj"
cap <- as.ltraj(xy = cap.alt[,c("x","y")], date = cap.alt$date,
                id = "Roe.Deer", typeII = TRUE,
                infolocs = cap.alt[,4:11])

#split it
cap <- cutltraj(cap, "dist > 100")

#add dummy column manually to one burst (these alterations will cause not allow all.equal = TRUE, but still are valid)
#infolocs(cap)[[1]]$dummy<-1
# dumb row names
#row.names(cap[[1]])<-11111:(11111+length(cap[[1]]$x)-1)

# storing column types with info
#info<-infolocs(cap)

###############################################################
#make iloc_df from infolocs fn()
iloc_df

types<-unlist(lapply(iloc_df,function(x) {class(x)[1]}))
types<-t(types)

# handle time types
tz<-types %in% c("POSIXct","POSIXlt","POSIXt")
tz2<-lapply(iloc_df[1,],function(x) {attr(x,"tzone")})
tz2<-t(tz2)

# make array of columns, types, and time zones
info_nm<-paste0("'{{",paste(names(iloc_df),collapse=","),"},{",
                paste(as.character(types),collapse=","),"},{",
                paste(as.character(tz2),collapse=","),"}}'")

# write info_nm to pgtraj.info_cols
dbSendQuery(conn,paste0("insert into traj.pgtraj (pgtraj_name, info_cols) 
                        VALUES ('captest',",
                        info_nm,");"))

# write infolocs table
dbWriteTable(conn,c("traj","iloc_captest"),value = iloc_df[FALSE,],row.names= FALSE)
for (n in names(iloc_df)[tz]) {
  dbSendQuery(conn,paste0("ALTER TABLE traj.iloc_captest ALTER COLUMN ",
                          dbQuoteIdentifier(conn,n),
                          " TYPE timestamp;"))
}
pgInsert(conn,c("traj","iloc_captest"),iloc_df,alter.names = FALSE)


#retrieve
get<-dbGetQuery(conn,"select unnest(info_cols[1:1]) as nms, 
                      unnest(info_cols[2:2]) as defs,
                      unnest(info_cols[3:3]) as tzs
                      from traj.pgtraj
                      where pgtraj_name = 'captest';") 

getinfo<-dbReadTable(conn,c("traj","iloc_captest"))

# match rownames
rownames(getinfo)<-rownames(iloc_df)
# drop unused levels
iloc_df$Status<-droplevels(iloc_df$Status)

# assign types
for (i in names(getinfo)) {
  att<-get[get$nms == i,]
  if (!is.na(att$tzs)) {
  getinfo[,i]<-eval(parse(text=paste0("as.",att$defs,
                                      "(as.character(getinfo[,i]),
                                      tz='",att$tzs,"')")))
  } else {
  getinfo[,i]<-do.call(paste0("as.",att$defs),list(getinfo[,i]))
  }
}
##############


all.equal(iloc_df,getinfo) 
# missing factor levels dropped
# times that are not in local TZ will be incorrect






pgTrajDrop(conn,"cap")
ltraj2pgtraj(conn,cap,infolocs = TRUE, overwrite=TRUE)

cap2<-pgtraj2ltraj(conn,pgtraj="cap")

all.equal(cap,cap2)
#infolocs have colum name differnces if columns were manually added, or column names match reserved names (they were changed)

#infolocs(cap)[[2]]<-NULL #doesn't work - infolocs cannot be removed except for all bursts (so length will always match)


####code below
#######
#######
#######
#send
iloc<-infolocs(ltraj)

for (i in 1:length(iloc)){
  
m<-iloc[[i]]

df<-data.frame(id=rep(NA,length(row.names(m))),burst=i,jsonval=rep(NA,length(row.names(m))),row.names=row.names(m))
for (j in row.names(m)) {
  js<-toJSON(m[j,])
  js<-substring(substring(js, 1, nchar(js)-1),2)
  df[j,]<-c(j,i,js)
}

suppressMessages(pgInsert(conn,name = "jsontest", data.obj = df))

}

# create/replace view to view infolocs normally

########
#retrive
# get distinct burst ids
blist<-dbGetQuery(conn,"SELECT distinct on (burst) burst, id from jsontest order by burst;")
ilist<-list()

for (b in row.names(blist)) {
  burst<-blist[b,]
# get distinct json keys for burst (each burst will always have the same keys for all rows)
#keys<-dbGetQuery(conn,"select jsonb_object_keys(jsonval) as keys from (select * from jsontest where [id = first row id]) a;")$keys
tmp.query<-paste0("select jsonb_object_keys(jsonval) as keys from (select * from jsontest where id = ",burst$id,") a;")
keys<-dbGetQuery(conn,tmp.query)$keys
keys2<-paste("jsonval->",dbQuoteString(conn,keys),dbQuoteIdentifier(conn,keys))
# returns data but order of columns is arbitrary (based on keys order)

#build query
tmp.query<-paste0("SELECT id, ", paste(keys2,collapse=","), " FROM jsontest WHERE burst = ",
                  dbQuoteString(conn,as.character(burst$burst))," ORDER BY id;")      #convert to char since burst could be integer or character

il_db<-suppressWarnings(dbGetQuery(conn,tmp.query))
row.names(il_db)<-il_db$id
il_db$id<-NULL

ilist[[b]]<-il_db
}

# reattach infolocs
cap2<-removeinfo(cap)
infolocs(cap2)<-ilist

# dummy column still there only in first
infolocs(cap2,"dummy")

i1<-infolocs(cap)[[1]]
i2<-infolocs(cap2)[[1]][colnames(i1)]
all.equal(i1,i2)

###
#using fromJSON
tab<-dbGetQuery(conn, "Select * from jsontest;")

#working
b<-apply(tab["jsonval"],MARGIN = 1,FUN = function(x) {as.data.frame(fromJSON(x["jsonval"],simplifyVector = TRUE))})
###  

current_search_path <- dbGetQuery(conn, "SHOW search_path;")


#fn arguments
pgtraj<-"cap_noinfo"
ltraj<-cap

schema<-"traj"
sql_query <- paste0("SET search_path TO ", dbQuoteIdentifier(conn,schema), ",public;")
dbSendQuery(conn,sql_query)

###
#data frame method

writeInfoFromLtraj<-function(conn,ltraj,pgtraj) {
  
  suppressPackageStartupMessages(requireNamespace("dplyr",quietly=TRUE))
  iloc<-infolocs(ltraj)

  #table_name
  iloc_nm<-paste0("z_infolocs_",pgtraj)

  ## data frame method
  #bind all list data frames 
  iloc_df<-dplyr::bind_rows(iloc)
  #iloc_df<-do.call("rbind",iloc) # doesn't work with different column names
  rnms<-row.names(do.call("rbind",ltraj))
  
  sql_query<-paste0("select s_i_b_rel.step_id,step.r_rowname as r_rowname931
                      from
                      pgtraj,
                      animal_burst,
                      s_i_b_rel,
                      step 
                      WHERE
                      pgtraj.id = animal_burst.pgtraj_id AND
                      animal_burst.id = s_i_b_rel.animal_burst_id AND
                      s_i_b_rel.step_id = step.id AND
                      pgtraj_name = ",dbQuoteString(conn,pgtraj),"
                      order by step_id;")
  
  step_ids<-dbGetQuery(conn,sql_query)
  
  iloc_df$r_rowname931<-as.character(rnms)
  
  fj<-dplyr::right_join(step_ids,iloc_df,by="r_rowname931")
  fj$r_rowname931<-NULL
  
  pgInsert(conn,name = c(iloc_nm),data.obj = fj,overwrite = TRUE, alter.names = FALSE)
  dbAddKey(conn,name = c(iloc_nm),type = "primary","step_id")
  
  # foreign key
  sql_query<-paste0("ALTER TABLE ",dbQuoteIdentifier(conn,iloc_nm), 
                    "ADD FOREIGN KEY (step_id) REFERENCES step (id)
                    ON UPDATE NO ACTION ON DELETE CASCADE;")
  dbSendQuery(conn,sql_query)
}

writeInfoFromLtraj(conn,ltraj,pgtraj)

getInfoFromPgtraj<-function(conn,pgtraj) {
  dbReadTable(conn,paste0("z_infolocs_",pgtraj))
}
  
# will delete removed rows (only in case of manual deletion, since regular method will overwrite table, and dropping pgtraj will drop table)

# also add a drop table to pgTrajDrop to remove infolocs table
pgTrajDrop(conn,pgtraj = pgtraj)

#1.store steps
#2. retrieve step.id, step.r_rowname,
