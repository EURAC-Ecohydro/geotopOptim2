# TODO: Add comment
# 
# Author: ecor
###############################################################################


NULL
#' Execution of GEOtop Hydrological Model and extraction of time series for soil dapth variables
#'

#' @param ... arguments for \code{\link{geotopExec}}
#' @param variable name of "Profile' variable to extract. Choose a subset of the default values. It is passed to \code{\link{geotopExec}} through \code{getKeywords} argument.
#' @param zformatter decimal formatters for soil layer identification. Default is \code{"z\%04d"}, expressed in centimeters. 
#' @param unit z coordinate measurement unit. GEOtop values expressed in millimeters which are converted to centimeters by default. Default is \code{c("centimeters","millimeters")}. Otherwise can be the ratio between the unit and one meter. 
#' @param geotop_unit z coordinate measurement unit used by GEOtop. Default is \code{millimeters}. 
#' 
#' 
#' @importFrom stringr str_detect
#' @importFrom stringr str_replace 
#' 
#' @seealso \code{\link{geotopExec}}
#' @export 
#' 
#' @examples
#' 
#' simpath <- system.file("Muntatschini_pnt_1_225_B2_004",package="geotopOptim")
#' bin <-   "/home/ecor/local/geotop/GEOtop/bin/geotop-2.0.0"
#' runpath <- "/home/ecor/temp/geotopOptim_tests"
#' 
#' vars <- c("SoilAveragedTempProfileFile",	"SoilLiqWaterPressProfileFile",
#' "SoilLiqContentProfileFile","SoilIceContentProfileFile")
#' 
#' vars <- "SoilLiqContentProfileFile"
#' 
#' out <- geotopZProfile(bin=bin,simpath=simpath,runpath=runpath,
#' 			clean=TRUE,variable=vars,data.frame=TRUE,level=1)
#' 
#' 
#' 
#' 


geotopZProfile <- function(...,variable=c("SoilAveragedTempProfileFile","SoilLiqWaterPressProfileFile","SoilLiqContentProfileFile","SoilIceContentProfileFile","AvailableSoilWaterContent"),
		zformatter="z%04d",unit=c("centimeters","millimeters"),geotop_unit="millimeters") {
	
	###print(zformatter)
	#####print(simpath)
	def_args <- as.character(formals(geotopZProfile)$variable) 
	
	unit <- unit[1]
	geotop_unit <- geotop_unit[1]
	
	if (unit=="millimeters") unit <- 0.001
	if (unit=="centimeters") unit <- 0.01
	
	if (geotop_unit=="millimeters") geotop_unit <- 0.001
	if (geotop_unit=="centimeters") geotop_unit <- 0.01
	
	if (all(variable %in% def_args) %in% c(FALSE,NA)) {
		
		index <- which(variable %in% def_args)
		
		error <- paste(variable[-index],collapse=" ")
		
		message <- paste("Variables not considered:",error,sep=" ")
		
		stop(message)
		
		
		
	}
	
	out <- geotopExec(...,getKeywords=variable)
	
	
	
	uf <- geotop_unit/unit
	
	out <- lapply(X=out,FUN=function(x,uf,zfrm){
				
				out <- x[,str_detect(names(x),"X")]
				#print("zfrm:")
				#print(zfrm)
				#print(names(x))
				zval <- as.numeric(str_replace(names(out),"X",""))*uf
				#print(zval)
				names(out) <- sprintf(zfrm,round(zval))
				
				return(out)
				
			},uf=uf,zfrm=zformatter)
	
	
	
	
	return(out)
	
}
