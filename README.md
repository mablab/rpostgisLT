rpostgisLT
==========

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/rpostgisLT)](https://CRAN.R-project.org/package=rpostgisLT)
[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
![](http://cranlogs.r-pkg.org/badges/rpostgisLT)
[![Build Status](https://travis-ci.org/mablab/rpostgisLT.svg?branch=master)](https://travis-ci.org/mablab/rpostgisLT)

This is the development area for the package `rpostgisLT`, a companion package to [`rpostgis`](https://github.com/mablab/rpostgis) and [`adehabitatLT`](https://CRAN.R-project.org/package=adehabitatLT), which provides functions to store `ltraj` (the trajectory data object from `adehabitatLT`) in a PostGIS-enabled PostgreSQL database.

The package was originally developed during Google Summer of Code 2016 by [Balázs Dukai](https://github.com/balazsdukai), with mentors Clément Calenge, [David Bucklin](https://github.com/dnbucklin), and [Mathieu Basille](https://github.com/basille).

## Installation of the released versions

You can install the latest released version from CRAN:

    install.packages("rpostgisLT")

You can use `update.packages()` to update to the latest CRAN version.

## Installation of the development version

A stable development version of the package will be available on the project's [Github page](https://github.com/mablab/rpostgisLT), which may be ahead the CRAN version. To install it, use the [`devtools`](https://CRAN.R-project.org/package=devtools) package from Hadley Wickham:

    library(devtools)
    install_github("mablab/rpostgisLT")
    
For the latest (possibly unstable) development version, use:

    install_github("mablab/rpostgisLT",ref="dev")

## Getting started

Of course, it is necessary to have PostgreSQL with PostGIS extension (ideally, versions 9.5+ and 2.2+, respectively; not tested on older versions). An installer for Windows, Linux and Mac systems can be found at [EnterpriseDB](http://www.enterprisedb.com/downloads/postgres-postgresql-downloads). During the install process, make sure to also install the PostGIS extension.

Once you are up and running with PostgreSQL, make sure to enable the PostGIS extension on each database, using:

    CREATE EXTENSION postgis;
    
You're now ready to use rpostgisLT. As a first test, you can try sending a sample `ltraj` dataset from the `adehabitatLT` package to the database. First, initiate the connection to the database:

    library(rpostgisLT)
    con <- dbConnect("PostgreSQL", dbname = <dbname>, host = <host>, user = <user>, password = <password>)
    
`rpostgisLT` stores `ltraj` in a data format called `pgtraj` (i.e., the PostGIS version of a trajectory). To store data, `rpostgisLT` builds the `pgtraj` data model in a dedicated schema in a database. All `rpostgisLT` functions specify the name of this schema to be `traj` by default, but it can be assigned any name you like (and you can have as many `pgtraj` schemas in a database as you would like). The functions that create `pgtraj` in `rpostgisLT` all will create this schema if it doesn't exist prior to building the `pgtraj`, but you can also create it ahead of time using the function `pgtrajSchema`.

    pgtrajSchema(con)
    
Since we don't specify the schema name, it defaults to "traj". The data model is now built in that schema. To learn more about the pgtraj data model, view the [rpostgisLT: Database model](https://github.com/mablab/rpostgisLT/wiki/The-traj-database-model) vignette.

We can now load a test dataset, and send it to the database using `ltraj2pgtraj`:

    data(capreochiz)
    head(capreochiz)
    ## Create an object of class "ltraj"
    cap <- as.ltraj(xy = capreochiz[,c("x","y")], date = capreochiz$date,
                id = "Roe.Deer", typeII = TRUE,
                infolocs = capreochiz[,4:8])
                
    # send to database
    ltraj2pgtraj(con, cap, pgtraj = "test_data")

`pgtraj` stored in the database can be re-imported as `ltraj` using the `pgtraj2ltraj` function:

    cap.db<-pgtraj2ltraj(con, "test_data")

`rpostgisLT` also can create pgtraj from data already stored in a database. Consider the following table storing animal relocations:
    
    -- this is SQL!!
    CREATE TABLE gps_data.relocations
    (
      gid serial PRIMARY KEY,                       -- primary key (unique integer value)
      animal_id character varying,                  -- unique animal name or ID
      acquisition_time timestamp with time zone,    -- timestamp of the relocations 
      x double precision,                           -- x coordinate
      y double precision,                           -- y coordinate
      geom geometry(Point,26917),                   -- POINT geometry object created from x and y with SRID = 26917
      error_est int,                                -- estimated error for the relocation
      land_cover int                                -- land cover code for the relocation
    );

To create one pgtraj named "test" from all the data in this table, you could use the `as_pgtraj` function with the following arguments:

    as_pgtraj(con,
              relocations_table = c("gps_data","relocations"),
              pgtrajs = "test",
              animals = "animal_id",
              relocations = "geom",
              timestamps = "acquisition_time",
              rids = "gid",
              note = "pgtraj containing all data from gps_data.relocations.")
              
Alternatively, you could create one pgtraj for each distinct animal_id in the table, by specifying the `pgtraj` argument as a column name, e.g.:

    as_pgtraj(con,
              relocations_table = c("gps_data","relocations"),
              pgtrajs = "animal_id",
              animals = "animal_id",
              relocations = "geom",
              timestamps = "acquisition_time",
              rids = "gid"
              )
              
You can also provide a column storing burst names, to further subdivide single animal's trajectories. Also, if you wish to only create pgtraj from a subset of the table, you can specify additional SQL using the `clauses` argument, as in this example, where only data from the year 2013 is selected:

    as_pgtraj(con,
              relocations_table = c("gps_data","relocations"),
              pgtrajs = "test_2013",
              animals = "animal_id",
              relocations = "geom",
              timestamps = "acquisition_time",
              rids = "gid",
              clauses = "WHERE extract(year FROM acquisition_time) = 2013"
              )

Finally, you can also attach information on locations (`infolocs` in an ltraj) using the `info_cols` argument. By default, the function assumes that these columns are also in `relocations_table`, but you can specify an alternate table (`info_table`) and its ID column (`info_rids`) that matches (JOINs) with the `rids` column in `relocations_table`.

    as_pgtraj(con,
              relocations_table = c("gps_data","relocations"),
              pgtrajs = "test_winfolocs",
              animals = "animal_id",
              relocations = "geom",
              timestamps = "acquisition_time",
              rids = "gid",
              info_cols = c("error_est","land_cover")
              )
              
All pgtraj can be directly imported into R `ltraj` using `pgtraj2ltraj`:

    test_2013<-pgtraj2ltraj(con, pgtraj = "test_2013")
    
To see more demonstrations on how `ltraj` can be modified, written to `pgtraj`, and re-imported into R without any data alteration, refer to the [Use Cases](https://github.com/mablab/rpostgisLT/wiki/Use-cases-for-the-rpostgisLT-package) vignette.