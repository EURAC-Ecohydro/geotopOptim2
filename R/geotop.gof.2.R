# TODO: Add comment
# 
# Author: ecor
###############################################################################


NULL 

#' GEOtop Goodnes of fit 
#' 
#' @param x vector with parameters to calibrate. See \code{param} in \code{\link{geotopExec}}.
#' @param run.geotop logical value. Default is \code{TRUE}. It it is \code{TRUE} GEOtop is runned though \code{\link{geotopExec}}.
#' @param target variables used for comparasion. The name of the variable must be in accordance with observation names returned by \code{\link{geotopLookUpTable}}.
#' @param uscale unit scale value for each target variable. (...)
#' @param when vector of time instants (class \code{\link{POSIXct}}. It is passed as \code{when} to \code{\link{geotopLookUpTable}}. If it is \code{NULL} all observation/simulated time duration is considered.
#' @param gof.mes measure (index) of godness of fit. The measure must be one of the row names of \code{\link{gof}}   
#' @param np exponent for te p-norm used. It must be an integer greater or equal to 1. It can be 1 (Manhattan), 2 (Euclidean) or \code{Inf} (Component of the Maximum absoulte value).  Default is 2.
#' @param nosuccess.return value thet function returns in case of non-0 (e.g. 1) exit of GEOtop simulation. Default is \code{Inf}.
#' @param ... further arguments
#' 
#' 
#  formals(geotopGOF)
#$x
#NULL
#
#$run.geotop
#[1] TRUE
#
#$target
#NULL
#
#$uscale
#NULL
#
#$whenGOF
#NULL
#
#$gof.mes
#[1] "RMSE"
#
#$np
#[1] 2
#
#$...
#
#
#> 

# 
# 
#  @param geotop.model a list with arguments for \code{\link{geotopZProfile}}. It is used if \code{sim} is \code{NULL}.
# @param approx.list list of arguments for \code{\link{approxfunDataFrame}} (optional) or \code{\link{integratefunDataFrame}} (in this case rember to specify the \code{zrange} argument).
# @param sim simulated data as a  object returned by \code{\link{geotopZProfile}}
# @param obs observed data 
# @param layer layers corresponding to soil depth at whch GOF indices are calculated
# @param weights vector of weights to assing to each layer, in case of a weigeted-averaged goodness-of-fit measure over all layers. It is \code{NULL} (Default) the gooness-of-fit measures are separately calculated for each layer. If it is \code{"uniform"}, the weights are uniformly distributed in all layers. 
# @param obs_field obs field used in the observation data frame. Deafault is \code{"mean"}, it is used in case varaiables are measured with different sensors at the same depth and location. 
# @param gof.mes string(s) containing adopted numerical goodness-of-fit measure. If it is \code{NULL} (Default), all mesasures returned by \code{\link{gof}} are calculated.
# @param gof.expected.value.for.optim expected value for goodness-of-fit mesure, e.g. 0 or 1. It is used if this function is called by \code{link{geotopPSO}},\code{link{hydroPSO}} or \code{link{optim}}.
# @param output_simulation logical value. If it is \code{TRUE}, function returns a list with the GOF values and the simulated time series
# @param names_par names of \code{x}
# @param temporary.runpath logical value. If it \code{TRUE} GEOtop is running in a temporary sub-directory, see \code{\link{geotopExec}}.  Default is \code{FALSE}.
# @param useSoilIntegratedValues logical values. Default is \code{FALSE}. If it is \code{TRUE} output is integrated with a soil thckness through \code{\link{integratefunDataFrame}}.
# @param ... further aguments for \code{\link{gof}} .
#' 
#' @export
#' @seealso \code{\link{geotopExec}},\code{\link{gof}}
#' 
#' @importFrom hydroGOF gof 
#' @examples 
#' 
#' wpath <- system.file('geotop-simulation/B2site',package="geotopOptim2")
#' bin <-   "/home/ecor/local/geotop/GEOtop/bin/geotop-2.0.0"
#' runpath <- "/home/ecor/temp/geotopOptim_tests"
#' 
#' var <- 'soil_moisture_content_50'
#' 
#' param <- c(N=1.4,Alpha=0.0021,ThetaRes=0.05) 
#' ssout <- geotopGOF(x=param,run.geotop=TRUE,bin=bin,
#' 			simpath=wpath,runpath=runpath,clean=TRUE,data.frame=TRUE,
#' 			level=1,intern=TRUE,temporary.runpath=TRUE)
#' 
#' 



# data(MuntatschiniB2)
# ## OBSERVATION PROCESSING
# 
# obs_SWC <- MuntatschiniB2[str_detect(names(MuntatschiniB2),"SWC")]
# zvalues <-  as.numeric(unlist(lapply(X=str_split(names(obs_SWC), pattern="", 
# 				n = Inf),FUN=function (x) {
# 					out <- as.numeric(x)
# 				    out <- out[!is.na(out)]
# 					out <- paste(out,collapse="")
# 				return(out)
# })))
# zformatter = "z%04d"
# names(obs_SWC) <- sprintf(zformatter,zvalues)
# obs_SWC <- lapply(X=obs_SWC,FUN=function(x){
# 
# 				if (length(dim(x))>1) {
# 					max <- apply(X=x,MARGIN=1,FUN=max,na.rm=TRUE)
# 					min <- apply(X=x,MARGIN=1,FUN=min,na.rm=TRUE)
# 					mean <- apply(X=x,MARGIN=1,FUN=mean,na.rm=TRUE)
# 					sd <- apply(X=x,MARGIN=1,FUN=sd,na.rm=TRUE)
# 				} else {
# 				
# 					mean <- as.vector(x)
# 				 	max <-  as.vector(x)
# 					min <-  as.vector(x)
# 					sd <- NA
# 
# 				}
# 				out <- data.frame(min=min,mean=mean,max=max,sd=sd)
# 
# 				out <- as.zoo(out)
#              index(out) <- as.POSIXlt(index(x))
# 
# 				return(out)
# 
# })
# ###########
# ###########
# 
# 
# simpath <- system.file("Muntatschini_pnt_1_225_B2_004",package="geotopOptim")
# bin <-   '/home/ecor/local/geotop/GEOtop/bin/geotop-2.0.0' 
###### '/Ubsers/ecor/local/bin/geotop_zh'
# runpath <- "/home/ecor/temp/geotopOptim_tests"
# 
# vars <- "SoilLiqContentProfileFile"
# 
# sim <- geotopZProfile(bin=bin,simpath=simpath,runpath=runpath,
# clean=TRUE,variable=vars,data.frame=TRUE,level=1,zformatter=zformatter,intern=TRUE)
# 
# gof <- geotopGOF(obs=obs_SWC,sim=sim[[vars[1]]],layer=c("z0005","z0020"))
# 
# gof
# 
# ### Use geotopGOF with an internal GEOtop simulation
# 
# ## create a list with arguments for geotopZProfile
# 
# x <- param <- c(N=1.4,Alpha=0.0021,ThetaRes=0.05) 
# 
# geotop.model <- list(bin=bin,simpath=simpath,runpath=runpath,
# clean=TRUE,variable=vars,data.frame=TRUE,level=1,zformatter=zformatter,intern=TRUE)
#  formals(geotopGOF)

# gof_geotop <- geotopGOF(x=x,obs=obs_SWC,geotop.model=geotop.model,layer=c("z0005","z0020"),gof.mes="KGE")
# 
# gof_geotop_ <- geotopGOF(x=x,obs=obs_SWC,geotop.model=geotop.model,layer=c("z0005","z0020"),gof.mes="KGE",useSoilIntegratedValues=TRUE,approx.list=list(zrange=c(0,500),rescaleWithDz=TRUE))
# 
# gof_geotop <- geotopGOF(x=x,obs=obs_SWC,geotop.model=geotop.model,layer=c("z0005","z0020"),gof.mes="KGE",useSoilIntegratedValues=FALSE,approx.list=list(zrange=c(0,500))
# ## PLAY WITH THE PLOTS!!!!
# 
# ###
# ###
# ###
# #modeled <- sim$SoilLiqContentProfileFile$z0020
# #observed <- obs_SWC$z0020
# #m <- merge(observed,modeled)
# #m <- m[!is.na(m$modeled),]
# ## Plotting standard deviation of observation vs observed range (max-min)
# 
# #plot(m$max-m$min,m$modeled-m$min)
# #abline(0,1)
# #points(m$mean-m$min,m$modeled-m$min,pch=2)
# #
# # 
# #plot(m$sd,m$mean-m$modeled)
# #
# #
# #
# ## plot(
#' #

######geotopGOF <- function(x=NULL,geotop.model=NULL,approx.list=list(),sim=NULL,obs,layer=c("z0005","z0020"),obs_field="mean",gof.mes=NULL,gof.expected.value.for.optim=NULL,weights=NULL,output_simulation=FALSE,names_par=NULL,useSoilIntegratedValues=FALSE,temporary.runpath=FALSE,multiaggr=TRUE,...) {
geotopGOF <- function(x=NULL,run.geotop=TRUE,target=NULL,uscale=NULL,when=NULL,gof.mes="RMSE",np=2,nosuccess.return=Inf,...)	{
	
	args <- list(param=x,run.geotop=run.geotop,when=when,...)
	
#	if (!is.null(args[["wpath"]])) {
#		args[["simpath"]] <- args[["wpath"]] 
#	
#	}  else if (!is.null(args[["simpath"]])) {
#		
#		args[["wpath"]] <- args[["simpath"]] 
#		
#	}
		
	lookup_tbl_names <- union(names(formals(geotopLookUpTable)),names(formals(approxfunDataFrame))) 
	lookup_tbl_names <- union(lookup_tbl_names,names(formals(approx)))
	geotop_exec_names <- names(formals(geotopExec))
	

	if (run.geotop==TRUE) {
		
	
		out <- try(do.call(what=geotopExec,args=args[names(args) %in% geotop_exec_names]),silent=TRUE)
		
	###	print("exit exec")
	###	print(out)
	}  else {
		
		out <- args[["wpath"]]
		exit <- attr(out,"output_execution") <- 0
	}
	
	
	
	args[["wpath"]] <- out
	
	exit <- attr(args[["wpath"]],"output_execution") 
	
	if (class(exit) %in% c("numeric","integer")) {
		
			status <- exit
		
	} else {
		
			status <- attr(exit,"status")
	}
	##print("status:")
	##print(status)
	if (is.null(status)) status <- 0
	if (is.na(status))   status <- 0
	
	if (status!=0) {
		
		msg <- sprintf("Goodness of fit non-successful (return %s))beceause of GEOtop non-0 exit : %s !",as.character(nosuccess.return),as.character(args[["wpath"]]))
		
		warning(msg)
		return(nosuccess.return)
		
	}
	
	out <- do.call(what=geotopLookUpTable,args=args[names(args) %in% lookup_tbl_names])
	
	
	observed_variables  <- attr(out,"observation_var")
	simulated_variables <- attr(out,"simulation_var")
	nvar <- intersect(observed_variables,simulated_variables)
	
	if (is.null(target)) {
		
		target <- nvar
	} else if (all(target %in% nvar)==FALSE) {
		
		cnt <- which(!(target %in% nvar))
		tmasg <- paste(target[cnt],collapse=" ")
		msg <- sprintf("The target vars %s are not included in the lookup table or observation is nissing and than removed!!")
		
		warning(msg)
		
		target <- target[-cnt]
		
		
	}
	
	if (is.null(uscale)) uscale <- NA
	if (is.na(uscale)) uscale <- 1
	if (length(uscale)==1) { 
	
		uscale <- array(uscale,length(target))
	} else if (length(uscale)!=length(target)) { 
	
		uscalem  <- paste(uscale,collapse=";")
		targetm  <- paste(target,collapse=";")
		
		msg <- sprintf("Mismatch target %s and uscale %s",targetm,uscalem)
		
		stop(msg)
		
				
				
	}
	names(uscale) <- target
	
	
	
	nobs <- paste("OBS",target,sep="_")
	nsim <- paste("SIM",target,sep="_")
	

	
	if (all(c(nobs,nsim) %in% names(out))==FALSE) {
		
		nn <- c(nobs,nsim)
		cnt <- which(!(nn %in% names(out)))
		tmasg <- paste(nn[cnt],collapse=" ")
		msg <- sprintf("The column %s are not present!!")
		
		stop(msg)
		
		
	}
	
	obs <- out[,nobs]
	sim <- out[,nsim]
	names(obs) <- target
	names(sim) <- target
	
	
	 #### http://stackoverflow.com/questions/18382883/what-is-the-right-way-to-multiply-data-frame-by-vector
	
	 ##sim <- data.frame(mapply(`*`,sim,uscale))
	if (length(uscale)>1) for (it in names(uscale)) {
		
		obs[,it] <- obs[,it]*uscale[it]
		sim[,it] <- sim[,it]*uscale[it]
	}  ## chiedere Samuel
	#str(obs)
	##str(sim)
	
	## INSERT whenGOF here 
	
	out <- gof(obs=obs,sim=sim)
	
	if (length(gof.mes)==1)  {
		
		
		out <- out[gof.mes,]
		
		expected.value <- gof(1:10,1:10)[gof.mes,]
		
		out <- abs(as.vector(out)-expected.value)
		
		
		if (np==Inf) {
			
			out <- max(out)
			
		} else { 
			out <- sum(out^np)^(1/np)
		}
		
	}
	
	### SEE   https://en.wikipedia.org/wiki/Multi-objective_optimization
	
	
	
	##nn <- names(out)[names(out) %in% c(nobs,nsim)]
	
	##out <- out[,nn]
	
	
#	
#	out <- out[,c(nobs,nsim)]
#	
#	if (!is.null(target_var) {
#	
#	t		
#		
#	} else { 
#		
#	
#	}
		
	return(out)
	
	
	}
	
