# TODO: Add comment
# 
# Author: ecor
###############################################################################
library(geotopbricks)

tz <- "Etc/GMT+1"

wpath <-  '/home/ecor/Dropbox/R-packages/geotopOptim2/inst/geotop-simulation/B2site_Cdev' 
inpts.file <- "geotop.inpts"
level <- 1

obs_data <- get.geotop.inpts.keyword.value(keyword="ObservationProfileFile", wpath=wpath,
		raster=FALSE,
		data.frame=TRUE,
		level=level, 
		date_field="Date12.DDMMYYYYhhmm.",
		tz=tz,inpts.file=inpts.file)


point_data <- get.geotop.inpts.keyword.value(keyword="PointOutputFile", wpath=wpath,
		raster=FALSE,
		data.frame=TRUE,
		level=level, 
		date_field="Date12.DDMMYYYYhhmm.",
		tz=tz,inpts.file=inpts.file) ###"Etc/GMT+1")


### LE_over_canopy =  Canopy_fraction * (LEg_veg + LEv) + (1 - canopy_fraction) * LEg_unveg

### H_over_canopy =  Canopy_fraction * (Hg_veg + Hv) + (1 - canopy_fraction) * Hg_unveg



### LE_over_canopy =  Canopy_fraction... * (LEg_veg.W.m2. + LEv.W.m2.) + (1 - Canopy_fraction...) * LEg_unveg.W.m2.

### H_over_canopy =  Canopy_fraction... * (Hg_veg.W.m2. + Hv.W.m2.) + (1 - Canopy_fraction...) * Hg_unveg.W.m2.



