library(devtools)
library(roxygen2)
library(rmarkdown)

pkg <- "rpostgisLT"

# Install, build, load the package
install(pkg)
#build(pkg, binary = FALSE)

library(rpostgisLT)
detach("package:rpostgisLT", unload=TRUE)
uninstall(pkg)

# Documentation
document(pkg)
#system("R CMD Rd2pdf --force /home/bdukai/Development/rpostgisLT/ -o /home/bdukai/Development/rpostgisLT/rpostgisLT.pdf")

render("C:/David/git/rpostgisLT/vignettes/data-model.Rmd")
render("C:/David/git/rpostgisLT/vignettes/use-cases.Rmd")

#install_github("mablab/rpostgis")
install("rpostgis")
detach("package:rpostgis", unload=TRUE)
uninstall("rpostgis")


