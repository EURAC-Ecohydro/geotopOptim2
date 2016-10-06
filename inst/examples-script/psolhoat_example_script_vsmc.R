#!/usr/bin/env Rscript
# file pso_example_script.R
#
# This script is an examples of a GEOtop lhoat via geotopOptim2
#
# author: Emanuele Cordano on 09-09-2015

#This program is free software: you can redistribute it and/or modify
#it under the terms of the GNU General Public License as published by
#the Free Software Foundation, either version 3 of the License, or
#(at your option) any later version.
#
#This program is distributed in the hope that it will be useful,
#but WITHOUT ANY WARRANTY; without even the implied warranty of
#MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#GNU General Public License for more details.
#
#You should have received a copy of the GNU General Public License
#along with this program.  If not, see <http://www.gnu.org/licenses/>.

###############################################################################



rm(list=ls())


library(zoo)
library(geotopOptim2)

Rpath <- '/home/ecor/Dropbox/R-packages/geotopOptim2/R' 
#lf <- list.files(Rpath,pattern=".R",full.names=TRUE)
#for (it in lf) source(it)

#source('/home/ecor/Dropbox/R-packages/geotopOptim2/R/geotop.pso.2.R')
#source('/home/ecor/Dropbox/R-packages/geotopOptim2/R/geotop.execution.R') 

set.seed(7988)

USE_RMPI <- FALSE 

if (USE_RMPI==TRUE) {
	library("parallel")

	library(Rmpi)
	require(snow)

	if (mpi.comm.rank(0) > 0) {
	    sink(file="/dev/null")
	#runMPIslave()
		slaveLoop(makeMPImaster())
		mpi.quit()
		
		
	}
	
	parallel <- "parallel"
    npart <- 16
	control <- list(N=5) ###list(maxit=5,npart=npart,parallel=parallel)
	
} else {
	
	parallel <- "none"
	npart <- 4
	control <- list(N=5,parallel="parallel",REPORT=10) ##list(maxit=5,npart=npart)
	
}




tz <- "Etc/GMT-1"

wpath <- system.file('geotop-simulation/B2site',package="geotopOptim2")


bin  <-  'geotop'
##bin  <-'/home/ecor/local/geotop/GEOtop/bin/geotop-2.0.0' 

## LOcal path where to write output for PSO
##runpath <- "/home/ecor/temp/geotopOptim_tests"
runpath <- Sys.getenv("GEOTOPOTIM2_TEMP_DIR")


geotop.soil.param.file <-  system.file('examples-script/param/param_pso_c001.csv',package="geotopOptim2") ###'/home/ecor/Dropbox/R-packages/geotopOptim/inst/examples_2rd/param/param_pso_test3.csv' 
geotop.soil.param <- read.table(geotop.soil.param.file,header=TRUE,sep=",",stringsAsFactors=FALSE)
lower <- geotop.soil.param$lower
upper <- geotop.soil.param$upper
x <- geotop.soil.param$suggested
names(lower) <- geotop.soil.param$name
names(upper) <- geotop.soil.param$name
if (!is.null(x)) names(x) <- geotop.soil.param$name






var <- c('soil_moisture_content_50','soil_moisture_content_200')


lhoat <- geotoplhoat(par=x,run.geotop=TRUE,bin=bin,
		simpath=wpath,runpath=runpath,clean=TRUE,data.frame=TRUE,
		level=1,intern=TRUE,target=var,gof.mes="RMSE",lower=lower,upper=upper,control=control)


file_lhoat <-  '~/local/geotopOptim2/inst/examples-script/outrda/lhoat_n.rda' 


save(lhoat,file=file_lhoat)


if (USE_RMPI==TRUE) mpi.finalize()
