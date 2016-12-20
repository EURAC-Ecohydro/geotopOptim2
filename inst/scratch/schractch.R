# TODO: Add comment
# 
# Author: ecor
###############################################################################
library(geotopOptim2)

Rfolder <-  '/home/ecor/Dropbox/R-packages/geotopbricks/R' 
Rfiles <- list.files(Rfolder,pattern=".R",full.name=TRUE)

for (it in Rfiles) source(it)

Rfolder <-  '/home/ecor/Dropbox/R-packages/geotopOptim2/R' 
Rfiles <- list.files(Rfolder,pattern=".R",full.name=TRUE)

for (it in Rfiles) source(it)


wpath <-  '/home/ecor/Dropbox/activity/2016/geotop_simulation/Latsch1_Calib_001' 

alldata <- geotopLookUpTable(wpath = wpath)
