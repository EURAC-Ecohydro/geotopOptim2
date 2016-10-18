#!/usr/bin/env Rscript
# file psolhoat_example_script_vsmle.R
#
# This script is an examples of a GEOtop lhoat via geotopOptim2
#
# author: Emanuele Cordano on 16-10-2016

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

## Set a seed for the random generation
set.seed(7988)
## This 'if' loop was introduced if hydroPSO has been worked on a MPI/parallel way 
## to optimize VSC performances.
## In these lines 'control' argument for 'lhoat' or 'hydroPSO' ('geotoplhoat' or 'geotopPSO') is set. See documentation: help(lhoat) or help(hydropso)  
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

## This 'if' loop was introduced to set the GEOtop binary file which be used in GEOtop 

USE_SE27XX <- FALSE

if (USE_SE27XX==TRUE) {
	
	##bin <- ' geotop-2.0.0'
	bin <- 'geotop_se27xx'
	
} else {
	
	bin  <-  'geotop_dev'
	
}  


## Set time zone: here GMT+1 (solar time in Rome/Berlin/Zurich/Brussel)

tz <- "Etc/GMT-1"

## Set the full path for GEOtop simulation template

wpath <- system.file('geotop-simulation/B2site',package="geotopOptim2")


## Set a temporary path where to run GEOtop simulations

runpath <- Sys.getenv("GEOTOPOTIM2_TEMP_DIR")

## Set/get  parameter calibartion values (upper and lower values and names)
## Here parameters are read from a CSV ascii files and then imported as a data frame

geotop.soil.param.file <-  system.file('examples-script/param/param_pso_cland002.csv',package="geotopOptim2") ###'/home/ecor/Dropbox/R-packages/geotopOptim/inst/examples_2rd/param/param_pso_test3.csv' 
geotop.soil.param <- read.table(geotop.soil.param.file,header=TRUE,sep=",",stringsAsFactors=FALSE)


## Parametrer value are saved as separate vactors: one for upper values , one for lower values, another for suggested value (only PSO not lhoat)
## Each vector elements must be named with parameter name in accordance with geotopOptim2 documention (see vignette)
lower <- geotop.soil.param$lower
upper <- geotop.soil.param$upper
x <- geotop.soil.param$suggested
names(lower) <- geotop.soil.param$name
names(upper) <- geotop.soil.param$name
if (!is.null(x)) names(x) <- geotop.soil.param$name


### Set Target Observed Variables (here are used the same names of observation file!)
### Set a scale value for each target values (here these values are proportial to its respenctive uncertainity error!) 
var <- c('soil_moisture_content_50','soil_moisture_content_200','latent_heat_flux_in_air','sensible_heat_flux_in_air')
uscale <- c(0.03,0.03,25,25)/0.03

names(var)  <- var
names(uscale) <- var


### Here 'lhoat' is triggered!

lhoat <- geotoplhoat(par=x,run.geotop=TRUE,bin=bin,
		simpath=wpath,runpath=runpath,clean=TRUE,data.frame=TRUE,
		level=1,intern=TRUE,target=var,gof.mes="RMSE",uscale=uscale,lower=lower,upper=upper,control=control)

### You can save the output in an RDA file!!! (if the following lines are uncommented) 
#file_lhoat <-  '~/local/geotopOptim2/inst/examples-script/outrda/lhoat_n.rda' 


#save(lhoat,file=file_lhoat)


if (USE_RMPI==TRUE) mpi.finalize()
