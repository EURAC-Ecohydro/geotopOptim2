# TODO: Add comment
# 
# Author: ecor
###############################################################################
rm(list=ls())

library(geotopOptim2)


source('/home/ecor/Dropbox/R-packages/geotopOptim2/R/geotop.gof.2.R')	
source('/home/ecor/Dropbox/R-packages/geotopOptim2/R/geotop.pso.2.R')


wpath <- '/home/ecor/activity/2016/eurac2016/idra/B2_BeG_017_DVM_001_test_1'
bin <-   "/home/ecor/local/geotop/GEOtop/bin/geotop-2.0.0"
runpath <- "/home/ecor/temp/geotopOptim_tests"
 
var <- c('soil_moisture_content_50','soil_moisture_content_200')

param <- c(N=1.4,Alpha=0.0021,ThetaRes=0.05) 
etime <- system.time( ssout <- geotopGOF(x=param,run.geotop=TRUE,bin=bin,
 			simpath=wpath,runpath=runpath,target=var,clean=TRUE,data.frame=TRUE,
 			level=1,intern=TRUE,temporary.runpath=TRUE))
 


 print(etime)

