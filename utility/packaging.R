library(devtools)
library(roxygen2)
library(rmarkdown)

pkg <- "rpostgisLT"

# Install, build, load the package
install(pkg)
#build(pkg, binary = FALSE)

library(rpostgisLT)
#detach(pkg, unload=TRUE)
#uninstall(pkg)

# Documentation
document(pkg)
system("R CMD Rd2pdf --force /home/bdukai/Development/rpostgisLT/ -o /home/bdukai/Development/rpostgisLT/rpostgisLT.pdf")

render("/home/bdukai/Development/rpostgisLT/vignettes/rpostgisLT.Rmd")
