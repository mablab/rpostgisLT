library(devtools)
library(roxygen2)
library(rmarkdown)

setwd("/home/bdukai/Development/rpostgisLT/")

#Note:
# For internal functions: delete Rd-file, leave out export-command in Namespace

#pkg <- "rpostgis"
pkg <- "rpostgisLT"
#create(pkg)
load_all(pkg)
document(pkg)

system("R CMD Rd2pdf --force /home/bdukai/Development/rpostgisLT/ -o /home/bdukai/Development/rpostgisLT/rpostgisLT.pdf")

install(pkg)
build(pkg, binary = FALSE)

library(rpostgisLT)
detach(package:rpostgisLT, unload=TRUE)
uninstall(pkg)

#devtools::use_vignette("rpostgisLT")
render("/home/bdukai/Development/rpostgisLT/vignettes/rpostgisLT.Rmd")

