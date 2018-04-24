## See https://github.com/odeleongt/postr/blob/master/README.md

library(rmarkdown)
library(flexdashboard)
library(webshot)
###library(postr)

## https://github.com/odeleongt/postr/pull/9/commits/fac03ce6aa7fb2a0df6f450ff076c0d4f79ca0ec
##rmarkdown::draft("my_poster.Rmd", template = "poster", package = "postr")
wpath <- '/home/ecor/Dropbox/R-packages/geotopOptim2/inst/poster' 
wpath <- '/STORAGE/projects/R-Packages/geotopOptim2/inst/poster' 


rmarkdown::render(sprintf("%s/my_poster/my_poster.Rmd",wpath))
postr::render(sprintf("%s/my_poster/erum_poster.html",wpath))