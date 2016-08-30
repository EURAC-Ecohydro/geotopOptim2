# TODO: Add comment
# 
# Author: ecor
###############################################################################
rm(list=ls())


library(zoo)
library(geotopOptim)

####
R <- '/home/ecor/Dropbox/R-packages/geotopOptim/R'
lapply(X=list.files(R,pattern=".R",full.names=TRUE),FUN=source)

####

obs <- '/home/ecor/activity/2016/eurac2016/Incarico_EURAC/Simulations/B2/B2_BeG_017_DVM_001/obs/observation.RData' 
tz <- "Etc/GMT-1"

load(obs)

SWC <- observation$hour[,c("soil_moisture_content_50","soil_moisture_content_200")]

names(SWC) <- c("z0005","z0020")
index(SWC) <- as.POSIXct(index(SWC))
index(SWC) <- as.POSIXlt(index(SWC),tz=tz)
vars <- "SoilLiqContentProfileFile"


wpath <- '/home/ecor/activity/2016/eurac2016/Incarico_EURAC/Simulations/B2/B2_BeG_017_DVM_001' 

###
layer <- "z0005"
###
###simpath <- system.file("Muntatschini_pnt_1_225_B2_004",package="geotopOptim")
bin <-   '/home/ecor/local/geotop/GEOtop/bin/geotop-2.0.0' 
##### '/Ubsers/ecor/local/bin/geotop_zh'
runpath <- "/home/ecor/temp/geotopOptim_tests"

vars <- "SoilLiqContentProfileFile"

sim <- geotopZProfile(bin=bin,simpath=wpath,runpath=runpath,
		clean=TRUE,variable=vars,data.frame=TRUE,level=1,intern=TRUE,tz=tz)

gof <- geotopGOF(obs=SWC,sim=sim[[vars[1]]],layer=names(SWC),multiaggr=FALSE)

gof



### Use geotopGOF with an internal GEOtop simulation

## create a list with arguments for geotopZProfile

x <- param <- c(N=1.4,Alpha=0.0021,ThetaRes=0.05) 

geotop.model <- list(bin=bin,simpath=wpath,runpath=runpath,
		clean=TRUE,variable=vars,data.frame=TRUE,level=1,tz=tz,intern=TRUE)

gof_geotop <- geotopGOF(x=x,obs=SWC,geotop.model=geotop.model,layer=layer,gof.mes="KGE",multiaggr=FALSE)

###  gof_geotop_ <- geotopGOF(x=x,obs=obs_SWC,geotop.model=geotop.model,layer=c("z0005","z0020"),gof.mes="KGE",useSoilIntegratedValues=TRUE,approx.list=list(zrange=c(0,500),rescaleWithDz=TRUE))

#