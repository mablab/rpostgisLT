rpostgisLT
==========

This is the development area for the package `rpostgisLT`, a companion package to [`rpostgis`](https://github.com/mablab/rpostgis) and [`adehabitatLT`](https://CRAN.R-project.org/package=adehabitatLT), which provides functions to store `ltraj` (the trajectory data object from `adehabitatLT`) in a PostGIS-enabled PostgreSQL database.

The package was originally developed during Google Summer of Code 2016 by [Balázs Dukai](https://github.com/balazsdukai), with mentors Clément Calenge, [David Bucklin](https://github.com/dnbucklin), and [Mathieu Basille](https://github.com/basille).

## Installation of the released versions

You can install the latest released version (0.4) from CRAN:

    install.packages("rpostgisLT")

You can use `update.packages()` to update to the latest CRAN version.

## Installation of the development version

A stable development version of the package will be available on the project's [Github page](https://github.com/mablab/rpostgisLT), which may be ahead the CRAN version. To install it, use the [`devtools`](https://CRAN.R-project.org/package=devtools) package from Hadley Wickham:

    library(devtools)
    install_github("mablab/rpostgisLT")
    
For the latest (possibly unstable) development version, use:

    install_github("mablab/rpostgisLT",ref="develop")

## Getting started

Of course, it is necessary to have PostgreSQL with PostGIS extension (ideally, versions 9.5+ and 2.2+, respectively; not tested on older versions). An installer for Windows, Linux and Mac systems can be found at [EnterpriseDB](http://www.enterprisedb.com/downloads/postgres-postgresql-downloads). During the install process, make sure to also install the PostGIS extension.

Once you are up and running with PostgreSQL, make sure to enable the PostGIS extension on each database, using:

    CREATE EXTENSION postgis;
    
You're now ready to used rpostgisLT. As a first test, you can try sending a test `ltraj` dataset from the `adehabitatLT` package to the database. First, initiate the connection to the database:

    library(rpostgisLT)
    con <- dbConnect("PostgreSQL", dbname = <dbname>, host = <host>, user = <user>, password = <password>)




