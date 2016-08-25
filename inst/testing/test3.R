# TODO: Add comment
# 
# Author: ecor
###############################################################################
## names(soil_data) <- names(gdepthsoil) 
rm(list=ls())

library(geotopOptim2)

set.seed(5454)

source('/home/ecor/Dropbox/R-packages/geotopOptim2/R/geotop.execution.R')	
source('/home/ecor/Dropbox/R-packages/geotopOptim2/R/geotop.lookup.table.R')
source('/home/ecor/Dropbox/R-packages/geotopOptim2/R/geotop.gof.2.R')	
source('/home/ecor/Dropbox/R-packages/geotopOptim2/R/geotop.pso.2.R')




wpath <- '/home/ecor/activity/2016/eurac2016/idra/B2_BeG_017_DVM_001_test_1'
bin <-   "/home/ecor/local/geotop/GEOtop/bin/geotop-2.0.0"
runpath <- "/home/ecor/temp/geotopOptim_tests"
####cca <<- NULL

var <- 'soil_moisture_content_50'

param <- c(N=1.4,Alpha=0.0021,ThetaRes=0.05) 
ssout <- geotopGOF(x=param,run.geotop=TRUE,bin=bin,
		simpath=wpath,runpath=runpath,clean=TRUE,data.frame=TRUE,
		level=1,intern=TRUE,target=var,temporary.runpath=TRUE)





x <- param <- c(N=1.4,Alpha=0.0021,ThetaRes=0.05,LateralHydrConductivity=0.021,NormalHydrConductivity=0.021) 
upper <- x*3

upper["LateralHydrConductivity"] <- 0.1
upper["NormalHydrConductivity"] <- 0.1

lower <- x/3
lower["N"] <- 1.1
lower["LateralHydrConductivity"] <- 0
lower["NormalHydrConductivity"] <- 0


control <- list(maxit=4,npart=2) ## Maximim 4 iterations!! 

pso <- geotopPSO(par=x,run.geotop=TRUE,bin=bin,
		simpath=wpath,runpath=runpath,clean=TRUE,data.frame=TRUE,
		level=1,intern=TRUE,target=var,gof.mes="KGE",lower=lower,upper=upper,control=control)

#####lhoat <- geotoplhoat(par=x,obs=obs_SWC,geotop.model=geotop.model,layer=c("z0020"),gof.mes="KGE",lower=lower,upper=upper,control=control)



