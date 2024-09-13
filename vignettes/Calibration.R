## ----echo=TRUE,eval=FALSE-----------------------------------------------------
#    library(geotopOptim2)
#    help(geotopExec,help_type="html")
#    help(get.geotop.keyword.inpts.value,help_type = "html")

## ----echo=TRUE,eval=FALSE-----------------------------------------------------
#  
#    help(geotopLookUpTable,help_type="html")

## ----echo=TRUE,eval=TRUE------------------------------------------------------
  wpath_sim <- system.file('geotop-simulation/B2site',package="geotopOptim2")
  inpts.file.path <- paste(wpath_sim,"geotop.inpts",sep="/")
  inpts <- readLines(inpts.file.path)
  writeLines(inpts)

## ----echo=TRUE,eval=TRUE------------------------------------------------------
  library(geotopbricks)
  
  obs <- get.geotop.inpts.keyword.value("ObservationProfileFile",wpath=wpath_sim,data.frame=TRUE,date_field='Date12.DDMMYYYYhhmm.')
 	str(obs)

## ----echo=TRUE,eval=TRUE------------------------------------------------------
  ltk <- get.geotop.inpts.keyword.value("ObservationLookupTblFile",wpath=wpath_sim,data.frame=TRUE,formatter="",col_sep=";")
  head(ltk)
  knitr::kable(ltk)

  

## ----echo=TRUE,eval=FALSE-----------------------------------------------------
#  
#    help(geotoGOF,help_type="html")
#    help(gof,help_type="gof")
#  

## ----echo=TRUE,eval=FALSE-----------------------------------------------------
#  
#    help(geotoGOF,help_type="html")
#    help(gof,help_type="gof")
#  

## ----fig.height=8, fig.width=6,echo=TRUE,results='hide',collapse=TRUE---------
#########/usr/bin/env Rscript
# file appendSmetData.R
#
# This script creates a graph of the package function and thair main external depencies
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

library(geotopOptim2)
library(raster)
library(igraph)
set.seed(123)
list_envs <- list(environment(geotopExec),environment(get.geotop.inpts.keyword.value),environment(gof),environment(hydroPSO),environment(writeLines),environment(read.table),environment(terrain))
list_envs <- c("geotopExec","get.geotop.inpts.keyword.value","gof","hydroPSO","writeLines","read.table") #,"terrain")
names(list_envs) <- c("geotopOptim2","geotopbricks","hydroGOF","hydroPSO","base","utils") #,"raster")
color <- c("red","green","blue","orange","yellow","white","brown")
names(color) <- names(list_envs)

###list_names <- lapply(X=list_envs,FUN=function(x){ls(env=x)})
list_names <- lapply(X=list_envs,FUN=function(x){ls(env=environment(get(x)))})## EC 20230405
list_df <- list()

for (it in names(list_envs)) {
	  
		list_df[[it]]	<- data.frame(funx=list_names[[it]],env=it,color=color[it],stringsAsFactors=FALSE)
	
	
}
df <- do.call(what=rbind,args=list_df)

##### SEMPLIFICATE DF

onlyfun <- list(hydroGOF=c("gof"),hydroPSO=c("hydroPSO","lhoat"),geotopbricks=c("get.geotop.inpts.keyword.value"),
		base=c("writeLines","readLines"),utils=c("read.table"),raster="raster") ### read.table was removed

for (it in names(onlyfun)) {
	
	cond  <- ((df$env==it) & (df$funx %in% onlyfun[[it]])) | (df$env!=it)
	df    <- df[cond,]
	
	
}		


fun_names <- df$funx
		

names(fun_names) <- fun_names

########################################
########################################
########################################
########################################
########################################
lfunx <- lapply(X=fun_names,FUN=function(x,nx) {
			o <- try(get(x),silent=TRUE)
			
			if (class(o)=="try-error") {
				
				o <- NA ### "It looks like a method!"
				return(o)
				
			}
			o <- formals(o)
		
			o <- lapply(X=o,FUN=as.character)
			
			o <- unlist(o)
			
			
			o <- o[o %in% nx]
			
			src <-  as.character(body(x))
			src <-  unlist(str_split(src, stringr::boundary("word")))
			nx <-  src[src %in% nx]
		
			o <- c(o,nx)
			o <- unique(o)
			
			
			
			
			return(o)
		
		},nx=fun_names)


for (it in names(lfunx)) {
	
	temp <- lfunx[[it]]
	ii <- which(temp!=it)
	temp <- temp[ii]
	nl <- length(temp)
	lfunx[[it]] <- array(c(rep(it,nl),temp),c(nl,2))
	
}
#####edges
edges <- do.call(rbind,lfunx)
vertices <- unique(edges)
#####
env_base <- "base;utils"
df$env[df$env=="base"]  <- env_base
df$env[df$env=="utils"] <- env_base
df$color[df$env==env_base] <- "white"
#####
color_ <- df$color
env_   <- df$env
names(color_) <- df$funx
names(env_) <- df$funx
######
gg <- graph_from_edgelist(edges)
vnames <- V(gg)$name
V(gg)$color <- color_[vnames]
vcodes <- sprintf("%02d",1:length(vnames))
names(vcodes) <- vnames
V(gg)$name <- vcodes
main <- "geotopOptim2  Internal Functions"
plot(gg,main=main)
legend("bottomleft",legend=unique(env_),fill=unique(color_),ncol=2)
legend("topleft",legend=paste(vcodes,vnames,sep=" : "),ncol=3,cex=0.6)

## ----echo=TRUE,eval=TRUE,collapse=TRUE----------------------------------------

#!/usr/bin/env Rscript
# file pso_example_script.R
#
# This script is an examples of a GEOtop calibration via geotopOptim2
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
	control <- list(maxit=5,npart=npart,parallel=parallel)
	
} else {
	
	parellel <- "none"
	npart <- 4
	control <- list(maxit=2,npart=npart)
	
}



## ----echo=TRUE,eval=TRUE,collapse=TRUE----------------------------------------
tz <- "Etc/GMT-1"

wpath <- system.file('geotop-simulation/B2site',package="geotopOptim2")



## ----echo=TRUE,eval=TRUE,collapse=TRUE----------------------------------------


##bin  <-'/home/ecor/local/geotop/GEOtop/bin/geotop-2.0.0' 
bin <-  "/home/ecor/local/sw/rendena100/geotop/geotop" 

## ----echo=TRUE,eval=TRUE,collapse=TRUE----------------------------------------


## LOcal path where to write output for PSO
#runpath <- "/home/lv70864/ecordano/temp/geotopOptim_tests"
runpath <- tempdir() ##"/home/ecor/temp/geotopOptim_tests"



## ----eval=FALSE---------------------------------------------------------------
#  
#  help(geotopExec,help_type="html")
#  help(geotopGOF,help_type="html")
#  help(geotopPSO,help_type="html")

## ----echo=TRUE,eval=TRUE,collapse=TRUE----------------------------------------

geotop.soil.param.file <-  system.file('examples_script/param/param_pso_c003.csv',package="geotopOptim2") 
geotop.soil.param <- read.table(geotop.soil.param.file,header=TRUE,sep=",",stringsAsFactors=FALSE)
lower <- geotop.soil.param$lower
upper <- geotop.soil.param$upper
suggested <- geotop.soil.param$suggested
names(lower) <- geotop.soil.param$name
names(upper) <- geotop.soil.param$name
names(suggested) <-  geotop.soil.param$name



knitr::kable(geotop.soil.param)


## ----echo=TRUE,eval=TRUE,collapse=TRUE----------------------------------------


var <- 'soil_moisture_content_50'
x <- (upper+lower)/2




## ----echo=TRUE,eval=FALSE,collapse=TRUE---------------------------------------
#  
#  pso <- geotopPSO(par=suggested,run.geotop=TRUE,bin=bin,
#  		simpath=wpath,runpath=runpath,clean=TRUE,data.frame=TRUE,
#  		level=1,intern=TRUE,target=var,gof.mes="RMSE",lower=lower,upper=upper,control=control)
#  
#  
#  
#  
#  
#  if (USE_RMPI==TRUE) mpi.finalize()
#  

