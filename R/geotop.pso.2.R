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
#' 
#' @param fn function to optimize (minimize or maximize). Default is \code{\link{geotopGOF}}. See \code{\link{hydroPSO}}. 
#' @param gof.mes string(s) containing adopted numerical goodness-of-fit measure. If it is \code{NULL} (Default), all mesasures returned by \code{\link{gof}} are calculated.
#' @param par model parameters. ((TO DO) See \code{\link{hydroPSO}},\code{\link{geotopGOF}} and \code{\link{geotopExec}}
#' @param final.run logical value. It is \code{TRUE} (default), simulated time series with optimal set of parameteers are added in the list object returned by the function.
#' @param upper,lower see  \code{upper} and \code{lowe} in \code{\link{hydroPSO}}
#' @param ... further arguments for \code{\link{hydroPSO}}.
#' @param hydroPSOfun  used function name of \code{hydroPSO} package: \code{\link{hydroPSO}} or \code{\link{lhoat}}.
#' @param temporary.runpath see \code{\link{geotopExec}}. Default is \code{TRUE}.
#' @details The function \code{fn}, in case it is different from the default value \code{\link{geotopGOF}} , must always have the arguments \code{gof.mes} and \code{gof.expected.value.for.optim}.
#' 
#' 
#' @export
#' 
####@description 
# @param gof.expected.value.for.optim expected value for Goodness-of-fit mesure, e.g. 0 or 1. It is used if this function is called by \code{link{geotopPSO}},\code{link{hydroPSO}} or \code{link{optim}}.
#' 
#' @rdname geotopPSO
#' 
#' @importFrom hydroPSO hydroPSO lhoat
#' 
#' 
#' @examples 
#' 
#'
#' 
#' wpath <- system.file('geotop-simulation/B2site',package="geotopOptim2") ####'/home/ecor/activity/2016/eurac2016/idra/B2_BeG_017_DVM_001_test_1'
#' bin <-  "/home/ecor/local/sw/rendena100/geotop/geotop" ### "/home/ecor/local/geotop/GEOtop/bin/geotop-2.0.0"
#' runpath <- tempdir() ###"/home/ecor/temp/geotopOptim_tests"
#' 
#' var <- 'soil_moisture_content_50'
#' 
#' param <- c(N=1.4,Alpha=0.0021,ThetaRes=0.05) 
#' ssout <- geotopGOF(x=param,run.geotop=TRUE,bin=bin,
#' 			simpath=wpath,runpath=runpath,clean=TRUE,data.frame=TRUE,
#' 			level=1,intern=TRUE,target=var,temporary.runpath=TRUE)
#' 
#' 
#' 
#' 
#' 
#' x <- param <- c(N=1.4,Alpha=0.0021,ThetaRes=0.05,
#'   LateralHydrConductivity=0.021,NormalHydrConductivity=0.021) 
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
#' control <- list(maxit=4,npart=2) ## Maximim 4 iterations!! 
#' 
#' \dontrun{
#' pso <- geotopPSO(par=x,run.geotop=TRUE,bin=bin,
#' 			simpath=wpath,runpath=runpath,clean=TRUE,data.frame=TRUE,
#' 			level=1,intern=TRUE,target=var,gof.mes="KGE",lower=lower,upper=upper,control=control)
#' }
#' #####lhoat <- geotoplhoat(par=x,obs=obs_SWC,... ## TO DO
#' 
#' @seealso \code{\link{hydroPSO}},\code{\link{gof}}\code{\link{lhoat}}
#'

geotopPSO <- function(par=NULL,fn=geotopGOF,gof.mes="KGE",final.run=TRUE,upper,lower,temporary.runpath=TRUE,...,hydroPSOfun=c("hydroPSO","lhoat")) {

###		if (is.charecter(fn)) fn <- get(fn)
#	   	if (is.null(gof.expected.value.for.optim))	gof.expected.value.for.optim <- NA
#		if (is.na(gof.expected.value.for.optim)) {
#			
#			x <- 1:100
#			gof.expected.value.for.optim <- gof(x,x)[gof.mes,1][1]
#			
#		}
		##print(gof.expected.value.for.optim)
		
		cond_param <- (length(lower)==length(upper)) & setequal(names(lower),names(upper))
		if (cond_param==TRUE) cond_param <- all(upper[names(lower)]>=lower) & cond_param 
		
		if (cond_param==FALSE) {
			
			stop("geotopPSO: inconstency between upper and lower values for calibration parameters!")
			
		}
		
		
		cond_par <- !is.null(par) & cond_param
		
		if (cond_par==TRUE) {
			
			cond_par <- (length(par)==length(upper)) & setequal(names(par),names(upper)) & cond_param
			if (cond_par==TRUE) {
				
				cond_par <- all(par[names(lower)]>=lower) & cond_par
				cond_par <- all(par[names(upper)]<=upper) & cond_par
				
				
			} 
			
			if (cond_par!=TRUE) {
		
				warning("geotopPSO: inconstency between par (IGNORED) and lower/upper  values for calibration parameters!")
				cond_par <- FALSE
				par <- lower
			}
			
	
		} else {
			
			
			
			par <- lower
		}
		
		
		
		
		
		upper <- upper[names(lower)]
		par <- par[names(lower)]
		
		###
		
		NLAYER <- upper["NumberOfSoilLayers"]
		if (length(NLAYER)==0) NLAYER <- NA ## EC 20200111
		print(NLAYER)
		geotop.model <- list(...)  #### [["geotop.model"]]
		#temporary.runpath <- geotop.model[["temporary.runpath"]]
		## ec 20160822
		if (is.null(temporary.runpath)) {
			
			temporary.runpath <- TRUE
		} else if (is.na(temporary.runpath)){
			
			temporary.runpath <- TRUE
			
		}
		
		
		## ec
		if (is.na(NLAYER)) {
			
		
			#### TO DO 
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
				if (is.null(soil.df)) {
				  
				   NLAYER <- 20 
				   ## print a message here
				} else {
				  
				  NLAYER <- nrow(soil.df)
				}
				
				
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
			par_all <- par[index(all)]
			
			upper <- upper[-index_all]
			lower <- lower[-index_all]
			par <-   par[-index_all]
			names(lower_all) <- str_replace(names(lower_all),"_ALL","_V_L%04d")
			names(upper_all) <- str_replace(names(upper_all),"_ALL","_V_L%04d")
			names(par_all) <- str_replace(names(par_all),"_ALL","_V_L%04d")
			
			names_all <- names(lower_all)
			
			for (it in names_all) {
			  print(NLAYER) ##
				itl <- sprintf(it,1:NLAYER)
				upper[itl] <- upper_all[it]
				lower[itl] <- lower_all[it]
				par[itl]   <- par_all[itl]
				
			}
			
			
			
		}
		
		##print("LOWER:")
		
		##print(lower)
		##print("UPPER:")
		##print(upper)
		
		###
#		cond_param <- (length(lower)==length(upper)) & setequal(names(lower),names(upper))
#		if (cond_param==TRUE) cond_param <- all(upper[names(lower)]>=lower) & cond_param 
#		if (cond_param==FALSE) {
#			
#			stop("geotopPSO: inconstency between upper and lower values for calibration parameters ()!")
#			
#		}
		
		
		###
		hydroPSOfun <- hydroPSOfun[1]
		
		if (hydroPSOfun=="hydroPSO") {
			
			
			if (cond_par==TRUE) {
					
				out <- hydroPSO(par=par,fn=fn,gof.mes=gof.mes,output_simulation=FALSE,upper=upper,lower=lower,names_par=names(upper),temporary.runpath=temporary.runpath,...)
				
				
			} else {
				
				out <- hydroPSO(fn=fn,gof.mes=gof.mes,output_simulation=FALSE,upper=upper,lower=lower,names_par=names(upper),temporary.runpath=temporary.runpath,...)
				
				
			}
			
			###out <- do.call(what=hydroPSO,args=hydroPSO_list)
			##print("out:")
			##print(out)
			if (final.run==TRUE) {
			
				
		 		out$gof <- fn(x=out$par,gof.mes=gof.mes,output_simulation=TRUE,temporary.runpath=FALSE,...)
		
				###out$sim <- do.call(what=approxfunDataFrame,args=approx.list)
			}
		
		} else if (hydroPSOfun=="lhoat") {
			
			
			out <- lhoat(fn=fn,gof.mes=gof.mes,output_simulation=FALSE,upper=upper,lower=lower,names_par=names(upper),temporary.runpath=temporary.runpath,...)
			
			
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