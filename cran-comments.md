## Test environments
* local ubuntu 16.04, R version 3.4.1 (2017-06-30)
* Windows Server 2012 R2 x64 (build 9600) (appveyor) R version 3.4.1 Patched (2017-08-27 r73146)
* Ubuntu 14.04.5 LTS (travis-ci), R version 3.4.1 (2017-01-27)

## R CMD check results
There were no ERRORs or WARNINGs.

There was 1 NOTE:
    Unexported objects imported by ':::' calls:
     ‘rpostgis:::dbBuildTableQuery’ ‘rpostgis:::dbConnCheck’
     ‘rpostgis:::dbExistsTable’ ‘rpostgis:::dbTableNameFix’
     See the note in ?`:::` about the use of this operator. 


## Downstream dependencies
There are currently no downstream dependencies for this package.
