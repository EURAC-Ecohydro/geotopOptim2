# TODO: Add comment
# 
# Author: ecor
###############################################################################
rm(list=ls())


library(zoo)
library(geotopOptim2)
set.seed(7988)


library("parallel")

library(Rmpi)
require(snow)

if (mpi.comm.rank(0) > 0) {
	sink(file="/dev/null")
#runMPIslave()
	slaveLoop(makeMPImaster())
	mpi.quit()
}





####
#R <- '/home/ecor/Dropbox/R-packages/geotopOptim/R'
#lapply(X=list.files(R,pattern=".R",full.names=TRUE),FUN=source)
####

####obs <- '/home/lv70864/ecordano/Simulations/B2_BeG_017_DVM_001/obs/observation.RData' 

tz <- "Etc/GMT-1"

###load(obs)

####SWC <- observation$hour[,c("soil_moisture_content_50","soil_moisture_content_200")]

###names(SWC) <- c("z0005","z0020")
###index(SWC) <- as.POSIXct(index(SWC))
###index(SWC) <- as.POSIXlt(index(SWC),tz=tz)
###vars <- "SoilLiqContentProfileFile"

## REMOVE JDF

ina <- which(as.numeric(as.character(index(SWC),format="%m")) %in% c(12,1,2))

SWC[ina,] <- NA

wpath <- '/home/lv70864/ecordano/Simulations/B2_BeG_017_DVM_001' ###/home/ecor/activity/2016/eurac2016/Incarico_EURAC/Simulations/B2/B2_BeG_017_DVM_001' 
###
#layer <- "z0005"
###
###simpath <- system.file("Muntatschini_pnt_1_225_B2_004",package="geotopOptim")
bin <-  'geotop' #### '/home/ecor/local/geotop/GEOtop/bin/geotop-2.0.0' 
##### '/Ubsers/ecor/local/bin/geotop_zh'
runpath <- "/home/lv70864/ecordano/temp/geotopOptim_tests"

###vars <- "SoilLiqContentProfileFile"



### Use geotopGOF with an internal GEOtop simulation

## create a list with arguments for geotopGOF
#
#x <- param <- c(N=1.4,Alpha=0.0021,ThetaRes=0.05,LateralHydrConductivity=0.021,NormalHydrConductivity=0.021) 
#upper <- x*3
#
#upper["LateralHydrConductivity"] <- 0.1
#upper["NormalHydrConductivity"] <- 0.1
#
#lower <- x/3
#lower["N"] <- 1.1
#lower["LateralHydrConductivity"] <- 0
#lower["NormalHydrConductivity"] <- 0

## create a list with arguments for geotopGOF
### Use geotopGOF with an internal GEOtop simulation

geotop.soil.param.file <- '/home/ecor/Dropbox/R-packages/geotopOptim/inst/examples_2rd/param/param_pso_test3.csv' 
geotop.soil.param <- read.table(geotop.soil.param.file,header=TRUE,sep=",",stringsAsFactors=FALSE)
lower <- geotop.soil.param$lower
upper <- geotop.soil.param$upper
names(lower) <- geotop.soil.param$name
names(upper) <- geotop.soil.param$name






geotop.model <- list(bin=bin,simpath=wpath,runpath=runpath,
		clean=TRUE,variable=vars,data.frame=TRUE,level=1,tz=tz,intern=TRUE)

 
#control=list(drty.out = cal_path, npart=48, maxit=iters,
#parallel="parallel", par.pkgs = c("gstat","caret","hydroGOF","sp"),
#		write2disk=TRUE,REPORT=100) )


control <- list(maxit=5,npart=16,parallel="parallel") ## Maximim 5iterations!! 

####pso <- geotopPSO(obs=SWC,geotop.model=geotop.model,layer=c("z0020"),gof.mes="RMSE",lower=lower,upper=upper,control=control,multiaggr=FALSE)

pso <- geotopPSO(par=x,run.geotop=TRUE,bin=bin,
		simpath=wpath,runpath=runpath,clean=TRUE,data.frame=TRUE,
		level=1,intern=TRUE,target=var,gof.mes="RMSE",lower=lower,upper=upper,control=control)


file_pso <-  '/home/lv70864/ecordano/pso_test3.rda'

save(pso,file=file_pso)


mpi.finalize()
