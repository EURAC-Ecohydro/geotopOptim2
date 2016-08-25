NULL
#' GEOtop calibration through Particle Swam Optimization
#'
#' This function performs a calibration or a senisitivity analyisis of 
#' the GEOtop Distributed Hydrological Model through a Particle Swam Optimization or LH-OAT method respectively.
#' This function is a wrapper of \code{\link{hydroPSO}} or \code{\link{lhoat}}. 
#' The use of either  \code{\link{hydroPSO}} or \code{\link{lhoat}} is declared by the argument \code{hydroPSOfun}.
#' 
#' 
#' 
#' 
#' @param par model parameters. See \code{\link{hydroPSO}},\code{\link{geotopGOF}} and \code{\link{geotopExec}}
#' @param fn function to optimize (minimize or maximize). Default is \code{\link{geotopGOF}}. See \code{\link{hydroPSO}}. 
#' @param gof.mes string(s) containing adopted numerical goodness-of-fit measure. If it is \code{NULL} (Default), all mesasures returned by \code{\link{gof}} are calculated.
#' @param gof.expected.value.for.optim expected value for Goodness-of-fit mesure, e.g. 0 or 1. It is used if this function is called by \code{link{geotopPSO}},\code{link{hydroPSO}} or \code{link{optim}}.
#' @param final.run logical value. It is \code{TRUE} (default), simulated time series with optimal set of parameteers are added in the list object returned by the function.
#' @param upper,lower see  \code{upper} and \code{lowe} in \code{\link{hydroPSO}}
#' @param ... further arguments for \code{\link{hydroPSO}}.
#' @param hydroPSOfun  used function name of \code{hydroPSO} package: \code{\link{hydroPSO}} or \code{\link{lhoat}}.
#' @details The function \code{fn}, in case it is different from the default value \code{\link{geotopGOF}} , must always have the arguments \code{gof.mes} and \code{gof.expected.value.for.optim}.
#' 
#' 
#' @export
#' 
####@description 
#' 
#' @rdname geotopPSO
#' 
#' @importFrom hydroPSO hydroPSO lhoat
#' 
#' 
#' @examples 
#' 
#' #' 
#' data(MuntatschiniB2)
#' ## OBSERVATION PROCESSING
#' 
#' obs_SWC <- MuntatschiniB2[str_detect(names(MuntatschiniB2),"SWC")]
#' zvalues <-  as.numeric(unlist(lapply(X=str_split(names(obs_SWC), pattern="", 
#' 				n = Inf),FUN=function (x) {
#' 					out <- as.numeric(x)
#' 				    out <- out[!is.na(out)]
#' 					out <- paste(out,collapse="")
#' 				return(out)
#' })))
#' zformatter = "z%04d"
#' names(obs_SWC) <- sprintf(zformatter,zvalues)
#' obs_SWC <- lapply(X=obs_SWC,FUN=function(x){
#' 
#' 				if (length(dim(x))>1) {
#' 					max <- apply(X=x,MARGIN=1,FUN=max,na.rm=TRUE)
#' 					min <- apply(X=x,MARGIN=1,FUN=min,na.rm=TRUE)
#' 					mean <- apply(X=x,MARGIN=1,FUN=mean,na.rm=TRUE)
#' 					sd <- apply(X=x,MARGIN=1,FUN=sd,na.rm=TRUE)
#' 				} else {
#' 				
#' 					mean <- as.vector(x)
#' 				 	max <-  as.vector(x)
#' 					min <-  as.vector(x)
#' 					sd <- NA
#' 
#' 				}
#' 				out <- data.frame(min=min,mean=mean,max=max,sd=sd)
#' 
#' 				out <- as.zoo(out)
#'              index(out) <- as.POSIXlt(index(x))
#' 
#' 				return(out)
#' 
#' })
#' ###########
#' ###########
#' 
#' 
#' simpath <- system.file("Muntatschini_pnt_1_225_B2_004",package="geotopOptim")
#' bin <-   "/home/ecor/local/geotop/GEOtop/bin/geotop-2.0.0"
#' runpath <- "/home/ecor/temp/geotopOptim_tests"
#' 
#' vars <- "SoilLiqContentProfileFile"
#' 

#' ### Use geotopGOF with an internal GEOtop simulation
#' 
#' ## create a list with arguments for geotopGOF
#' 
#' x <- param <- c(N=1.4,Alpha=0.0021,ThetaRes=0.05,LateralHydrConductivity=0.021,NormalHydrConductivity=0.021) 
#' upper <- x*3
#' 
#' upper["LateralHydrConductivity"] <- 0.1
#' upper["NormalHydrConductivity"] <- 0.1
#' 
#' lower <- x/3
#' lower["N"] <- 1.1
#' lower["LateralHydrConductivity"] <- 0
#' lower["NormalHydrConductivity"] <- 0
#' 
#' 
#' geotop.model <- list(bin=bin,simpath=simpath,runpath=runpath,
#' clean=TRUE,variable=vars,data.frame=TRUE,level=1,zformatter=zformatter,intern=TRUE)
#' control <- list(maxit=4,npart=2) ## Maximim 4 iterations!! 
#' 
#' pso <- geotopPSO(par=x,obs=obs_SWC,geotop.model=geotop.model,layer=c("z0020"),gof.mes="KGE",lower=lower,upper=upper,control=control)
#' 
#' lhoat <- geotoplhoat(par=x,obs=obs_SWC,geotop.model=geotop.model,layer=c("z0020"),gof.mes="KGE",lower=lower,upper=upper,control=control)
#' 
#' 
#' @seealso \code{\link{hydroPSO}},\code{\link{gof}}\code{\link{lhoat}}
#'

geotopPSO <- function(fn=geotopGOF,gof.expected.value.for.optim=NA,gof.mes="KGE",weights="uniform",final.run=TRUE,upper,lower,...,hydroPSOfun=c("hydroPSO","lhoat")) {

###		if (is.charecter(fn)) fn <- get(fn)
	   	if (is.null(gof.expected.value.for.optim))	gof.expected.value.for.optim <- NA
		if (is.na(gof.expected.value.for.optim)) {
			
			x <- 1:100
			gof.expected.value.for.optim <- gof(x,x)[gof.mes,1][1]
			
		}
		##print(gof.expected.value.for.optim)
		
		cond_param <- (length(lower)==length(upper)) & setequal(names(lower),names(upper))
		if (cond_param==TRUE) cond_param <- all(upper[names(lower)]>=lower) & cond_param 
		if (cond_param==FALSE) {
			
			stop("geotopPSO: inconstency between upper and lower values for calibration parameters!")
			
		}
		upper <- upper[names(lower)]
		###
		
		NLAYER <- upper["NumberOfSoilLayers"]
		
		geotop.model <- list(...)[["geotop.model"]]
		temporary.runpath <- geotop.model[["temporary.runpath"]]
		
		
		if (is.na(NLAYER)) {
			
		
			
			if (!is.null(geotop.model)) {
				
				simpath <- geotop.model$simpath
				inpts.file <- geotop.model[["inpts.file"]]
				SoilType <- geotop.model[["SoilType"]]
				##print(simpath)
				if (is.null(inpts.file)) inpts.file <- formals(geotopExec)$inpts.file
				
				
				if (is.null(SoilType)) {
						
						
						level <- geotop.model[["level"]]
						if (is.null(level)) level <- formals(geotopExec)$level
						SoilType <- tryCatch(extract.geotop.value.fromMap("SoilMapFile",wpath=simpath,inpts.file=inpts.file)[level,"SoilMapFile"],error=function(e) {NA})
						if (is.null(SoilType)) SoilType <- NA
						if (is.na(SoilType)) SoilType <- formals(geotopExec)$SoilType
						
				}
					
				 ####SoilType <- formals(geotopExec)$SoilType
				
				soil.df <- get.geotop.inpts.keyword.value("SoilParFile",wpath=simpath,inpts.file=inpts.file,data.frame=TRUE,level=SoilType)
				NLAYER <- nrow(soil.df)
				
				
				
			} else {
				
				
				
				NLAYER <- 20 
				
			}
			
			
			
			
		}
			
			
		cond_all <- (str_detect(names(lower),"_ALL"))
		names(cond_all) <- names(lower)
		
		if (any(cond_all)==TRUE) {
			
			index_all <- which(cond_all)
			upper_all <- upper[index_all]
			lower_all <- lower[index_all]
			upper <- upper[-index_all]
			lower <- lower[-index_all]
			
			names(lower_all) <- str_replace(names(lower_all),"_ALL","_V_L%04d")
			names(upper_all) <- str_replace(names(upper_all),"_ALL","_V_L%04d")
			names_all <- names(lower_all)
			
			for (it in names_all) {
			
				itl <- sprintf(it,1:NLAYER)
				upper[itl] <- upper_all[it]
				lower[itl] <- lower_all[it]
				
				
			}
			
			
			
		}
		
		##print("LOWER:")
		
		##print(lower)
		##print("UPPER:")
		##print(upper)
		
		###
		cond_param <- (length(lower)==length(upper)) & setequal(names(lower),names(upper))
		if (cond_param==TRUE) cond_param <- all(upper[names(lower)]>=lower) & cond_param 
		if (cond_param==FALSE) {
			
			stop("geotopPSO: inconstency between upper and lower values for calibration parameters ()!")
			
		}
		
		
		###
		hydroPSOfun <- hydroPSOfun[1]
		
		if (hydroPSOfun=="hydroPSO") {
			out <- hydroPSO(fn=fn,gof.mes=gof.mes,gof.expected.value.for.optim=gof.expected.value.for.optim,weights=weights,output_simulation=FALSE,upper=upper,lower=lower,names_par=names(upper),temporary.runpath=temporary.runpath,...)
			##print("out:")
			##print(out)
			if (final.run==TRUE) {
			
				
		 		out$gof <- fn(x=out$par,gof.mes=gof.mes,gof.expected.value.for.optim=gof.expected.value.for.optim,weights=NULL,output_simulation=TRUE,temporary.runpath=FALSE,...)
		
				###out$sim <- do.call(what=approxfunDataFrame,args=approx.list)
			}
		
		} else if (hydroPSOfun=="lhoat") {
			
			out <- lhoat(fn=fn,gof.mes=gof.mes,gof.expected.value.for.optim=gof.expected.value.for.optim,weights=weights,output_simulation=FALSE,upper=upper,lower=lower,names_par=names(upper),temporary.runpath=temporary.runpath,...)
			
			
		} else {
			
			out <- NULL
		}
		
		
		return(out)
		



}

NULL

#'
#' @rdname geotopPSO
#' @export
#' 


geotoplhoat <- function(...) {
	
	out <- geotopPSO(...,hydroPSOfun="lhoat")
	return(out)
	
	
	
}