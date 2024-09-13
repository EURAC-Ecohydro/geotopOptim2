NULL
#' Extract variable from an object returend by \code{\link{geotopLookUpTable}}
#' 
#' @param x objectect returned by \code{\link{geotopLookUpTable}} with \code{merge==TRUE}
#' @param InputVar discover variable
#' @param aggregate aggregate option. Default it is \code{c("hourly","daily","monthly","yearly")} and it is considered the first element. 
#' @param aggregate_fun aggregate function for \code{InputVar} and \code{Add_InputVar} respectively. Deafault is \code{c("mean","sum")}.
#' @param ... further arguments
#' 
#' @export 
#' 
#' @note It is assumed that \code{x} already contains hourly aggregated time series, so in case \code{aggregare=="hourly"} no aggragetion is calculated.
#'
#' 
#' @seealso \code{\link{geotopLookUpTable}}
#' 
#' @examples 
#' wpath <- system.file('geotop-simulation/B2_BeG_017',package="geotopOptim2")
#' wpath <- system.file('geotop-simulation/B2site',package="geotopOptim2")
#' ex <- geotopLookUpTable(wpath = wpath)
#'  
#' eout <- aggregateVar(ex)
#' dout <- aggregateVar(ex,aggregate="daily")
#' mout <-  aggregateVar(ex,aggregate="monthly")
#' 
#' 
#' 


aggregateVar <- function(x,InputVar=NULL,aggregate=c("hourly","daily","monthly","yearly"),aggregate_fun=c("mean","sum"),...)  {
	
	obsnames <- attr(x,"observation_var")
	simnames <- attr(x,"simulation_var")
	
	nnames <- intersect(obsnames,simnames)
	
	aggregate <- aggregate[1]
	if (is.null(InputVar)) {
		
		InputVar <- nnames[1]
		
	}
	
	
	name <- paste(c("OBS","SIM"),InputVar,sep="_")
	
	
	
	
	
	
	
	out <- x[,1:2]
	out[,] <- NA
	names(out) <- c("observation", "simulation")
	
	nn <- c(name)[1:3]

	names(nn) <- names(out)
	onn <- nn
	nn <- nn[(nn %in% names(x))]
	out[,names(nn)] <- x[,nn]
	attr(out,"var_name") <- onn 
	
	if (aggregate!="hourly") {
		
		## 1  fun_aggregeta [2]
		## 2 and 3 fun_aggregate [3]
		
		
		tz <- sprintf("Etc/%s",format(index(out[1]),format="%Z"))
		
		if (aggregate=="daily") aggregate <- "%Y-%m-%d"
		if (aggregate=="monthly") aggregate <- "%Y-%m"
		if (aggregate=="yearly") aggregate <- "%Y"
		
		byc <- format(index(out),format=aggregate)
		
		by <- as.POSIXlt(byc,tz=tz,format=aggregate)
		
		out <- as.data.frame(out)
		lout <- list()
		cfun <- c(1,1)
		for (c in 1:ncol(out)) {
			
			lout[[c]] <- tapply(X=out[,c],INDEX=byc,FUN=get(aggregate_fun[cfun[c]]),na.rm=TRUE)
			
		}
		names(lout) <- names(out)
		
		lout <- as.data.frame(lout)
		
		lout <- as.zoo(lout)
		###str(lout)
		if (aggregate=="%Y-%m") rownames(lout) <- paste(rownames(lout),15,sep="-")
		if (aggregate=="%Y") rownames(lout) <- paste(rownames(lout),5,15,sep="-")
		index(lout) <- as.POSIXct(rownames(lout),tz=tz,format="%Y-%m-%d")
		
		out <- lout
		
		
		
	}
	
	return(out)
	
	
	
	
} 