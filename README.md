# Managing and visualizing movement data with PostGIS and R

Mentoring organization during the Google Summre of Code 2016:

**R project for statistical computing**

Mentors:
 
 + Clément Calenge
 + David Bucklin
 + Mathieu Basille
 
Developer:

 + Balázs Dukai

The project develops the integration of R and PostGIS for managing movement trajectories. The focus is on streamlining the workflow for biologists to store and process animal trajectories in PostGIS and analyze these in R, thus utilizing the strengths of both software. Therefore the main outcome is a new R package rpostgisLT, which will simplify the processing of location datasets into trajectories in PostGIS and provide full integration with the R package adehabitatlt data type ltraj.

**Project directory structure**

/inst – contains non-R scripts required by the package

/man – .Rd files of the package manual

/R – .R scripts of the package

/tests – test scripts used by the package

/utility – additional scripts that help the development process

/vignettes – package vignettes

## Installation of the development version

You need to use the package devtools from Hadley Wickham:

    library(devtools)
    install_github("mablab/rpostgis")
