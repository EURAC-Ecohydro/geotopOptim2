# TODO: Add comment
# 
# Author: ecor
###############################################################################


library(shiny)
library(rmarkdown)
library(markdown)

rmdpath <- '/home/ecor/Dropbox/R-packages/geotopOptim2/inst/examples_animation'
rmdfile <- paste(rmdpath,'ViewSimulation.Rmd',sep="/") 
htmlfile <- paste(rmdpath,'ViewSimulation.html',sep="/") 

#markdownToHTML(file=rmdfile,output=htmlfile)

run(rmdfile)

