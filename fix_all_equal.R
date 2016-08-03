drv <<- dbDriver("PostgreSQL")
conn <- dbConnect(drv, user="bdukai", password="ERBAgoNd1#", dbname="rpostgisLT",
        host="localhost")

for (i in 1:length(ibexraw)) {
    x <- dl(ld(ibexraw[i]))
    pgtraj <- paste0("ibexraw", i)
    ltraj2pgtraj(conn, ltraj = x, schema = "traj", pgtraj = pgtraj)
    x_re <- pgtraj2ltraj(conn, schema = "traj", pgtraj = pgtraj)
    attr(x_re, "regular") <- FALSE
    print(paste("all.equal() result:",all.equal(x, x_re)))
}

# gives schema error because there are duplicate ids -> one animal has two bursts
for (i in 1:length(puechcirc)) {
    x <- dl(ld(puechcirc[i]))
    pgtraj <- paste0("puechcirc", i)
    ltraj2pgtraj(conn, ltraj = x, schema = "traj", pgtraj = pgtraj)
    x_re <- pgtraj2ltraj(conn, schema = "traj", pgtraj = pgtraj)
    attr(x_re, "regular") <- TRUE
    print(paste("all.equal() result:",all.equal(x, x_re)))
}

for (i in 1:length(albatross)) {
    x <- dl(ld(albatross[i]))
    pgtraj <- paste0("albatross", i)
    ltraj2pgtraj(conn, ltraj = x, schema = "traj", pgtraj = pgtraj)
    x_re <- pgtraj2ltraj(conn, schema = "traj", pgtraj = pgtraj)
    attr(x_re, "regular") <- FALSE
    print(paste("all.equal() result:",all.equal(x, x_re)))
}

for (i in 1:length(porpoise)) {
    x <- dl(ld(porpoise[i]))
    pgtraj <- paste0("porpoise", i)
    ltraj2pgtraj(conn, ltraj = x, schema = "traj", pgtraj = pgtraj)
    x_re <- pgtraj2ltraj(conn, schema = "traj", pgtraj = pgtraj)
    attr(x_re, "regular") <- FALSE
    print(paste("all.equal() result:",all.equal(x, x_re)))
}

dbSendQuery(conn, "DROP SCHEMA traj CASCADE;")



