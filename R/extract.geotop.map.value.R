NULL
#' Getting values from a GEOtop Simulation Map
#' 
#' This function extracts the values of one or more GEOtop maps in certain check point. 
#' 
#' @param key keywords related to the maps
#' @param xykeys keywords keywords relate to the selected chck points where to extract the values. Default is \code{c("Listpoint","HeaderCoordinatePointX","HeaderCoordinatePointY")}, where the first keyword refers to the file with XY Coordinates of the check points, the second and the third ones refer to  the headers of X and Y Coordinates respectively. 
#' @param wpath,inpts.file,... arguments that are passed to \code{\link{get.geotop.inpts.keyword.value}}
#'  
#' 
#' @importFrom raster cellFromXY
#' 
#' @export
#' 
#' @seealso  \code{\link{get.geotop.inpts.keyword.value}}
#' 
#' @examples 
#' 
#' wpath <- "/Users/ecor/Dropbox/R-packages/geotopOptim-stuff-SHALINI/exercise/simulation_template"
#' 
#' points <- extract.geotop.value.fromMap(key="SoilMapFile",wpath=wpath)
#' points0 <- extract.geotop.value.fromMap(key=c("SoilMapFile","DemFile"),wpath=wpath)
#' 
#' nopoints <- extract.geotop.value.fromMap(key="NoSoilMapFile",wpath=wpath)
#' nopoints0 <- extract.geotop.value.fromMap(key=c("NoSoilMapFile","DemFile"),wpath=wpath)
#' nopoints1 <- extract.geotop.value.fromMap(key=c("NoSoilMapFile","NoDemFile"),wpath=wpath)
#' 



#
#
#if (is.na(SoilType)) {
#	
#	soil_map <- get.geotop.inpts.keyword.value("SoilMapFile",raster=TRUE,wpath=rundir,inpts.file=inpts.file)
#	xy_pointfile <- get.geotop.inpts.keyword.value("ListPoints",data.frame=TRUE,wpath=rundir,inpts.file=inpts.file)
#	
#	##	PointFile = "ListPoints"
#	
#	
#}


extract.geotop.value.fromMap <- function (key,xykeys=c("PointFile","HeaderCoordinatePointX","HeaderCoordinatePointY"),wpath,inpts.file="geotop.inpts",...) {
	
	
	## AGGIUNGERE LE POSSIBILITA' CHE NON CI SIA LAMAPPA 
	out <- NULL
	map <- tryCatch(get.geotop.inpts.keyword.value(key,raster=TRUE,wpath=wpath,inpts.file=inpts.file,...),error=function(e) {NULL})
	
	
	xy_p <- tryCatch(get.geotop.inpts.keyword.value(xykeys[1],data.frame=TRUE,formatter="",wpath=wpath,inpts.file=inpts.file),error=function(e) {NULL})
	if (is.null(xy_p)) xy_p <- data.frame(x=NA,y=NA)
	xheader <- tryCatch(get.geotop.inpts.keyword.value(xykeys[2],wpath=wpath,inpts.file=inpts.file),error=function(e) {NULL})
	yheader <- tryCatch(get.geotop.inpts.keyword.value(xykeys[3],wpath=wpath,inpts.file=inpts.file),error=function(e) {NULL})
	##str(xy_p)
	##print(xy_p)
	
	out <- xy_p ###xy_pointfile
	if (is.null(map)) {
		
		out[,key] <- NA
		
	} else if (class(map)=="list")  {
		
		nonull <- length(which(!sapply(X=map,FUN=is.null)))
		
		
		if (nonull>0) {
			
			icells <- cellFromXY(map[!sapply(X=map,FUN=is.null)][[1]],xy_p[,c(xheader,yheader)])
		
			for (it in names(map)) {
			
				out[,it] <- map[[it]][icells]
			
			
			} 
		} else {
			
			
			out[,key] <- NA
		}
		
		
	}	else {
		
		icells <- cellFromXY(map,xy_p[,c(xheader,yheader)])
		
		out[,key] <- map[icells]
		
	}

	
	
	
	
	
	
	
	
	
	
	return(out)
	
	
	
}
