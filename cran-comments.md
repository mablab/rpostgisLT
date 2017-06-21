## Test environments
* local Windows 7 install, R 3.4.0
* win-builder, R Under development (unstable) (2017-06-19 r72808)
* Debian 8.8, R 3.3.3

## R CMD check results
There were no ERRORs, WARNINGS, or NOTEs.

Due to the requirement of a PostgreSQL/PostGIS enabled database for function in this package, all package tests are run locally and excluded from the R build.

There is an error on the CRAN Package Check Results for the R version 3.4.0 Patched (2017-06-17 r72808) on platform x86_64-apple-darwin15.6.0 (64-bit). This is due to an error with the package `RPostgreSQL` on that build (which `rpostgisLT` depends on), preventing it from being installed.


## Downstream dependencies
There are currently no downstream dependencies for this package.