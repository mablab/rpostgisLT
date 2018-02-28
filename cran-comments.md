## Test environments
* local ubuntu 16.04 (Linux Mint 18.3), R version 3.4.3 (2017-11-30)
* Windows Server 2012 R2 x64 (build 9600) (appveyor) R version 3.4.3 Patched (2018-02-26 r74307)
* Ubuntu 14.04.5 LTS (travis-ci), R version 3.4.2 (2017-01-27)

## R CMD check results
There were no ERRORs or WARNINGs.

There was 1 NOTE:
    Unexported objects imported by ':::' calls:
     ‘rpostgis:::dbBuildTableQuery’ ‘rpostgis:::dbConnCheck’
     ‘rpostgis:::dbExistsTable’ ‘rpostgis:::dbTableNameFix’
     See the note in ?`:::` about the use of this operator.

rpostgisLT is developed by the same people as rpostgis, and the unexported objects
are not part of the rpostgis release yet

## Downstream dependencies
There are currently no downstream dependencies for this package.
