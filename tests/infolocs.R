library(rpostgis)
library(jsonlite)
conn<-dbConnect("PostgreSQL",dbname="rpostgis",user="postgres",password="",host="localhost")

dbSendQuery(conn,"DROP TABLE jsontest;")
dbSendQuery(conn,"CREATE TABLE jsontest (id int primary key, burst int, jsonval jsonb);")

## example of an object with an attribute "infolocs"
data(capreochiz)
head(capreochiz)

## Create an object of class "ltraj"
cap <- as.ltraj(xy = capreochiz[,c("x","y")], date = capreochiz$date,
                id = "Roe.Deer", typeII = TRUE,
                infolocs = capreochiz[,4:8])

#split it
cap <- cutltraj(cap, "dist > 100")

#add dummy column manually to one burst
infolocs(cap)[[1]]$dummy<-1

ltraj<-cap

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


#using fromJSON
tab<-dbGetQuery(conn, "Select * from jsontest;")

#working
b<-apply(tab["jsonval"],MARGIN = 1,FUN = function(x) {as.data.frame(fromJSON(x["jsonval"],simplifyVector = TRUE))})
     

