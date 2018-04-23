## See https://github.com/odeleongt/postr/blob/master/README.md

library(rmarkdown)
library(flexdashboard)
library(webshot)


## https://github.com/odeleongt/postr/pull/9/commits/fac03ce6aa7fb2a0df6f450ff076c0d4f79ca0ec
rmarkdown::draft("my_poster.Rmd", template = "poster", package = "postr")
rmarkdown::render("my_poster/my_poster.Rmd")
postr::render("my_poster.html")