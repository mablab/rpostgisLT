library(devtools)
library(roxygen2)
library(rmarkdown)

pkg <- "rpostgisLT"

# Install, build, load the package
install(pkg)
build(pkg, binary = FALSE)
devtools::check_built(path = "/home/bdukai/Development/rpostgisLT_0.4.0.tar.gz", check_version=TRUE)
build_win(pkg)
use_build_ignore(c(".dbeaver-data-sources.xml", "./tests",
"Scripts", "cran-comments.md", "rpostgisLT.pdf", "rpostgisLT_rhub.txt",
  "utility"), escape=FALSE, pkg = pkg)

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


