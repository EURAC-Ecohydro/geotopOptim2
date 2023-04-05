NULL
#' Integration on the soil layers
#' 
#' This function integrates the values of a \code{zt}-variable quantity  within a certain thickness. 
#' 
#' 
#' @param df data frame or 'zoo' object with (modeled) time series. 
#' @param z vector of soil layer depths. Default is \code{NULL} and is taken from column names of \code{df}.
#' @param zrange  vector of soil layer thickness within which \code{df} values are integrated (or averaged)  with \code{\link{approx}} and then integrated with  \code{\link{integrate}}.
#' @param formatter character string with decimal formatter. It is used if \code{z} or {zout} are string vectors. Default is \code{"z\%04d"}. 
#' @param factor unit factor used for conversion of  \code{z} or {zout} from String vector to numeric vector and viceversa.
#' @param rescaleWithDz logical value. If is \code{TRUE} the integrated value is rescaled with soil thickness (mean). Default is \code{FALSE} and the value is not rescaled.
#' @param ... further argument for \code{\link{approx}}
#' 
#' 
#' @export
#' 
#' @return The values of the \code{df} quantity interpolated in the \code{zout} soil depths. 
#' 
#' @seealso \code{\link{approx}},\code{\link{integrate}}
#' 
#' @importFrom stats integrate
#' 
#' 
#' 
#' @examples 
#'
#' ## NOTHING 
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
# bin <-  "/home/ecor/local/sw/rendena100/geotop/geotop" ### "/home/ecor/local/geotop/GEOtop/bin/geotop-2.0.0"
# runpath <- ...
# 
# vars <- "SoilLiqContentProfileFile"
# 
# sim <- geotopZProfile(bin=bin,simpath=simpath,runpath=runpath,
# clean=TRUE,variable=vars,data.frame=TRUE,level=1,zformatter=zformatter,intern=TRUE)[[vars]]
# 
# system.time(sim_int <- integratefunDataFrame(df=sim,formatter=zformatter))
# 
# 


integratefunDataFrame <- function(df,z=NULL,zrange=c(0,500),formatter="z%04d",factor=10,rescaleWithDz=FALSE,...) {
	
	
	
	
	
	if (is.null(z)) {
		
		zc <- names(df)
		zcf <- str_replace_all(zc,"[a-z]","")
		z <- as.numeric(zcf)*factor
				
	} else {
		
		zc <- sprintf(formatter,z/factor)
		
		
	}
	

	if (ncol(df)!=length(z)) {
		
		stop("Error in integratefunDataFrame: data frame columns do not match with z.")	
	}
	


	



	approxm <- function(zout,df=NULL,z,...) { 
		
		df <- as.vector(df)
	
		as.vector(approx(x=z,y=df,xout=zout,yleft=df[1],yright=rev(df)[1],...)$y)
	}
	
	

#	print(df[1,])
	print(approxm(20,df=as.vector(df[1,]),z=z))
	print(approxm(0.001,df=df[1,],z=z))
	zintegrate <- function(df=NULL,z,zrange,...) {integrate(f=approxm,lower=zrange[1],upper=zrange[2],df,z,...)$value}
	
	out <- df[,1]
	out[] <- apply(X=as.matrix(df),MARGIN=1,FUN=zintegrate,z=z,zrange=zrange,...)
#		
	
	if (rescaleWithDz==TRUE)  out <- out/diff(zrange) 


	return(out)
	
}	
