NULL
#' Interpolation on the soil layers
#' 
#' This function interpolates the values of a \code{zt}-variable quantity in layers of a carteain depth. 
#' 
#' 
#' @param df data frame or 'zoo' object with (modeled) time series. 
#' @param z vector of soil layer depths. Default is \code{NULL} and is taken from column names of \code{df}.
#' @param zout vector of soil layer depth at which \code{df} values are interpoleted with \code{\link{approx}}
#' @param formatter character string with decimal formatter. It is used if \code{z} or {zout} are string vectors. Default is \code{"z\%04d"}. 
#' @param factor unt factor used for conversion of  \code{z} or {zout} from String vector to numeric vector and viceversa.
#' @param ... further argument for \code{\link{approx}}
#' 
#' @export
#' 
#' @return The values of the \code{df} quantity interpolated in the \code{zout} soil depths. 
#' 
#' @importFrom stats approx
#' 
#' 
#' @seealso \code{\link{approx}}
#' @examples 
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
# bin <-   "/home/ecor/local/geotop/GEOtop/bin/geotop-2.0.0"
# runpath <- "/home/ecor/temp/geotopOptim_tests"
# 
# vars <- "SoilLiqContentProfileFile"
# 
# sim <- geotopZProfile(bin=bin,simpath=simpath,runpath=runpath,
# clean=TRUE,variable=vars,data.frame=TRUE,level=1,zformatter=zformatter,intern=TRUE)[[vars]]
# 
# 
# sim_approx <- approxfunDataFrame(df=sim,zout=names(obs_SWC),formatter=formatter)
# 
# plot(sim$z0020)
# plot(sim_approx$z0020)
# 
# 


approxfunDataFrame <- function(df,z=NULL,zout,formatter="z%04d",factor=10,...) {
	
	
	
	
	
	if (is.null(z)) {
		
		zc <- names(df)
		z <- str_replace_all(zc,"[a-z]","")
		z <- as.numeric(z)*factor
		
		
	} else {
		
		zc <- sprintf(formatter,z/factor)
		
		
	}
	

	if (ncol(df)!=length(z)) {
		
		stop("Error in approxfunDataFrame data frame columns do not match with z.")	
	}
	
	if (is.character(zout)) {
		
		zoutc <- zout
		zout <- str_replace_all(zout,"[a-z]","")
		zout <- as.numeric(zout)*factor
		
	} else {
		
		zoutc <- sprintf(formatter,zout/factor) 
		
		
		
	}
	
	
	names(zout) <- zoutc
	
	approxm <- function(df=NULL,z,zout,...) {as.vector(approx(x=z,y=df,xout=zout,...)$y)}
	
	nzl <- length(zout)
	
	if (nzl>1) {
		#print("DF:")
	
		#print("END")
		out <- df[,rep_len(1:2,length.out=nzl)] 
	
		#names(out) <- zoutc
		#print(zout)
		#str(out)
		out[,] <- NA
	
		for (i in 1:nzl) {
			out[,i] <- apply(X=as.matrix(df),MARGIN=1,FUN=approxm,z=z,zout=zout[i],...)
		}
	} else if (nzl==1) {
		
		out <- df[,1]
		out[] <- apply(X=as.matrix(df),MARGIN=1,FUN=approxm,z=z,zout=zout[1],...)
		
		
	} else {
		
		out <- NULL
		
	}
	
	
	
	
	return(out)
	
	
}