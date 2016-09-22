NULL
#'  Read point output from GEOtop for verification of the model results
#' 
#'	Read point output from GEOtop for verification of the model results
#' 
#' @param wpath			path into simulation folder
#' @param inpts.file name of the GEOtop configuration file. Default is \code{"geotop.inpts"}
#' @param obs				zoo object, data frame, with specific names of variables used for validate the model results; name conventions according to CF Standard Name Table \url{http://cfconventions.org}.
#' @param lookup_tbl_observation lookup table with observed and GEOtop-simulated variables
#' @param tz time zone, see \code{\link{get.geotop.inpts.keyword.value}}. Default is \code{"Etc/GMT-1"}.
#' @param level check point index. Default is 1. See \code{\link{get.geotop.inpts.keyword.value}}
#' @param soil_files				boolean, \code{TRUE}: soil files are provided as GEOtop input. \code{FALSE}: soil is parameterized in the geotop.inpts file
#' @param save_rData				(OBSOLATE)boolean, if \code{TRUE} (default) data is stored in working directory (simulation folder)
#' @param merge.output     logical. If it is \code{TRUE}, the output is a marged in a unique \code{zoo} object. 
#' @param when vector of time instants (class \code{\link{POSIXct}}.  If it is \code{NULL} all observation/simulated time duration is considered.
#' @param ... further arguments for \code{\link{approxfunDataFrame}}
#' 
#' @export
#' @importFrom stringr str_split str_replace str_locate_all str_sub str_detect boundary
#' @importFrom zoo index as.zoo 'index<-'
#' @importFrom geotopbricks get.geotop.inpts.keyword.value  
#' @importFrom utils read.table  

# @importFrom grDevices dev.off grey grey.colors pdf rainbow rgb 
# @importFrom graphics abline axis barplot grid legend lines par plot polygon text title
# @importFrom stats aggregate ecdf qqplot sd time window 
# @importFrom utils data head read.csv read.table tail
# 
# @importFrom ggplot2 geom_text 
#' 
#' 
#' 
#' @seealso \code{\link{get.geotop.inpts.keyword.value}}, \code{\link{approxfunDataFrame}}
#' @author 	Johannes Brenner,Emanuele Cordano
#' 
#' @examples 
#'  ## TO DO 
#' 
#' 
#' wpath <- '/home/ecor/activity/2016/eurac2016/idra/B2_BeG_017_DVM_001_test_1' 
#' 
#' out <- geotopLookUpTable(wpath = wpath , save_rData = TRUE)
#' 
#' 
#' 
#' 

#
#
#




# Function to load GEOtop point simulation output based on observations

#  wpath <- "/run/user/1000/gvfs/smb-share:server=sdcalp01.eurac.edu,share=data2/Simulations/Simulation_GEOtop_1_225_ZH/Vinschgau/SimTraining/BrJ/HiResAlp/1D/Montecini_pnt_1_225_B2_007/"
#  wpath <- "/run/user/1000/gvfs/smb-share:server=sdcalp01.eurac.edu,share=data2/Simulations/Simulation_GEOtop_1_225_ZH/Vinschgau/SimTraining/BrJ/MonaLisa/1D/Kaltern/sim006"
#  data("observations_B2")
#  
#  load(file.path(wpath, "obs", "observation.RData"))
#  names(observation) <- c("hour", "day")
#  obs <- observation
#  ### ggExtra Geotop_VisSoilWaterRet_gg
#  obs   <- list(hour=B2_h, day=B2_d)

geotopLookUpTable <- function(wpath, obs="ObservationProfileFile", lookup_tbl_observation="ObservationLookupTblFile",soil_files=TRUE, save_rData=TRUE,tz="Etc/GMT-1",level=1,inpts.file="geotop.inpts",merge.output=TRUE,when=NULL,...)

{
  # source lookup_tbl
 # lookup_tbl_observation <- NULL ##ec 20150526
 ## data(lookup_tbl_observation)
 
 
  if (is.null(lookup_tbl_observation)) {
  	lookup_tbl_observation_csv <- system.file('tool/lookup_tbl_observation.csv',package="geotopAnalysis") 
  	lookup_tbl_observation     <- read.table(lookup_tbl_observation_csv,sep=";",header=TRUE,stringsAsFactors=FALSE)	 
  
  } else if (identical(lookup_tbl_observation,"ObservationLookupTblFile")) {
	 
	  lookup_tbl_observation <- get.geotop.inpts.keyword.value('ObservationLookupTblFile',wpath=wpath,data.frame=TRUE,formatter="",inpts.file=inpts.file,col_sep=";")
 	
  }
  
#  return(lookup_tbl_file_observation)
#   lookup_tbl_observation <- apply(lookup_tbl_observation, 2, as.character)
#   lookup_tbl_observation <- as.data.frame(lookup_tbl_observation)
  
# check observation data
#  if (any(names(obs)=="hour") & any(names(obs)=="day"))  {
#	  Donly <- which(! dimnames(obs$day)[2][[1]] %in% dimnames(obs$hour)[2][[1]]) 
#  } else {
#	  Donly <- NULL
#  }
# get x- , y-coordinates of output points
#  if (level!=1) {	
#  	listpoints <- get.geotop.inpts.keyword.value("PointFile",wpath=wpath,data.frame=TRUE,inpts.file=inpts.file) 
###  print(listpoints)
###  stop("CIAO")
###  if (file.exists(file.path(wpath,"listpoints.txt")))
#  	if (!is.null(listpoints)) {
##    listpoints <- read.csv(file.path(wpath,"listpoints.txt"))
#     	xpoints <- listpoints$xcoord
#     	ypoints <- listpoints$ycoord
#   	} else {
#    	xpoints <- get.geotop.inpts.keyword.value("CoordinatePointX",wpath=wpath,numeric=TRUE,inpts.file=inpts.file)
#    	ypoints <- get.geotop.inpts.keyword.value("CoordinatePointY",wpath=wpath,numeric=TRUE,inpts.file=inpts.file)
#  	}
#  }


# read data from point file (preperation)
#  if (!is.null(Donly)) {
#	  base <- obs$day 
#  } else {
#	  base <- obs$hour
#  }
  
#  df_names <- as.data.frame(dimnames(base)[2][[1]])
#  names(df_names) <- "name"
#  varPointIn <- merge(df_names, lookup_tbl_observation, by.x="name", by.y = "obs_var")
#  varPointIn_what_direct <- varPointIn$geotop_what[varPointIn$geotop_where=="PointOutputFile"]
#  varPointIn_name_direct <- varPointIn$name[varPointIn$geotop_where=="PointOutputFile"]
#  

 # level <- 1:length(xpoints)
# read point data with specified keyword  
  if (obs=="ObservationProfileFile") {
	  obs_data <- get.geotop.inpts.keyword.value(keyword="ObservationProfileFile", wpath=wpath,
			  raster=FALSE,
			  data.frame=TRUE,
			  level=level, 
			  date_field="Date12.DDMMYYYYhhmm.",
			  tz=tz,inpts.file=inpts.file) ###"Etc/GMT+1")
  } else {
	  
	 obs_data <- obs 
  }
 
  
  ### FIND KEYWORDS
  
  
  #### INSERT SOIL DATA 
  
  geotop_where_a <- paste(as.character(lookup_tbl_observation$geotop_where),collapse=";")
  ikeys <- str_locate_all(geotop_where_a,boundary("word"))[[1]]
 
  geotop_where <- sprintf("s%05d",1:nrow(ikeys))
  
  for (i in 1:nrow(ikeys)) {
	  
	geotop_where[i] <- str_sub(geotop_where_a,start=ikeys[i,1],end=ikeys[i,2])
	  
  }
  geotop_where <- unique(geotop_where)
  geotop_where <- geotop_where[str_detect(geotop_where,"File")]
 
  
  
  ##### INSERT SOIL DATA
  geotop_soil_where <- geotop_where[str_detect(geotop_where,"Soil")]
  if (length(geotop_soil_where)>1) {
	  
  	names(geotop_soil_where) <- geotop_soil_where
	zlayer.formatter="z%04d"

  	soil_data <- lapply(X=geotop_soil_where,FUN=get.geotop.inpts.keyword.value,wpath=wpath,
		  raster=FALSE,
		  data.frame=TRUE,
		  level=level, 
		  date_field="Date12.DDMMYYYYhhmm.",
		  tz=tz,inpts.file=inpts.file,zlayer.formatter=zlayer.formatter) ###"Etc/GMT+1")
  
  
     
#  	print("BA")
#  	str(soil_data)
#	print("ba")
	
  	ivarsoil <- integer(0)
 	for (it in geotop_soil_where) {
	  
		ivarsoil <-   c(ivarsoil,which(str_detect(lookup_tbl_observation$geotop_where,it)))
		ivarsoil <- unique(ivarsoil)
	  
  	}
  
  #####ivarsoil <- which(any(str_detect(lookup_tbl_observation$geotop_where %in% geotop_soil_where)
  ##TEMPORARY:
  #return(ivarsoil)
  
  	
  ####
  	gnamessoil <- as.character(lookup_tbl_observation$geotop_where[ivarsoil])
	
 	gdepthsoil <-  as.character(lookup_tbl_observation$geotop_what[ivarsoil])
	
  	onamessoil <- as.character(lookup_tbl_observation$obs_var[ivarsoil])
	
	
  	names(gnamessoil) <- onamessoil
  	names(gdepthsoil) <- onamessoil
	
	###return(list(soil_data=soil_data,gnamessoil=gnamessoil,gdepthsoil=gdepthsoil,onamessoil=onamessoil))
	index_time <- index(soil_data[[1]])

	
	soil_data <- lapply(X=soil_data,FUN=function(x,gdepthsoil,...) {
				
					out <- as.data.frame(x)
					###names__ <- names(out)
					### SE GOF previous version 
					###print(gdepthsoil)
					out <- approxfunDataFrame(df=out,zout=gdepthsoil,...)
				    #### VEDI APPROXDATAFRAMEFUN !!!!!!
					
					
					
					###
					return(out)
			
				},gdepthsoil=gdepthsoil)
	##TEMPORARY:
	###return(list(soil_data=soil_data,gnamessoil=gnamessoil,gdepthsoil=gdepthsoil,onamessoil=onamessoil))
	

  	evals_gnamessoil <- lapply(X=gnamessoil,FUN=function(x,envir){
			  
			  e <- parse(text=x)
			 # str(envir)
			  o <- eval(e,envir=envir)
			  o <- as.data.frame(o)
			  return(o)
		  },envir=soil_data)
  
 
	evals_gnamessoils <- mapply(text=gdepthsoil,envir=evals_gnamessoil,FUN=function(text,envir){ eval(parse(text=text),envir=envir)})
  	

  	
  	soil_data <- as.data.frame(evals_gnamessoils)

	
  	names(soil_data) <- names(gdepthsoil)
  	soil_data <- as.zoo(soil_data)
	
	
  	index(soil_data) <- index_time
  	
  
  	
  
   } else {
	   
	   
	 soil_data <- NULL
   }
  
  
 
  
  
  #####  END INSERT SOILDATA
  if ("PointOutputFile" %in% geotop_where) {
  
  
  		point_data <- get.geotop.inpts.keyword.value(keyword="PointOutputFile", wpath=wpath,
                                                 raster=FALSE,
                                                 data.frame=TRUE,
                                                 level=level, 
                                                 date_field="Date12.DDMMYYYYhhmm.",
                                                 tz=tz,inpts.file=inpts.file) ###"Etc/GMT+1")
  
  									 
										 
										 
										 
#LWnet.W.m2. and SWnet.W.m2. is below the canopy, see also LE and H 
  
   
  		ivarpoint <- which(lookup_tbl_observation$geotop_where=="PointOutputFile") #### & !str_detect(lookup_tbl_observation$geotop_what,"="))
  
  
  		gnamespoint <- as.character(lookup_tbl_observation$geotop_what[ivarpoint])
  		onamespoint <- as.character(lookup_tbl_observation$obs_var[ivarpoint])
  		names(gnamespoint) <- onamespoint
		
  ### UNIT
 		onamespoint <- lookup_tbl_observation$obs_var[ivarpoint]
  		
  
  		evals_gnamespoint <- lapply(X=gnamespoint,FUN=function(x,envir){
			
			  e <- parse(text=x)
			 
		  	  o <- eval(e,envir=envir)
			  return(o)
		  },envir=as.data.frame(point_data))
  
  
  	
  
  	time_axis <- index(point_data)
  	point_data <- as.data.frame(evals_gnamespoint)
  	point_data <- as.zoo(point_data)
  	index(point_data) <- time_axis
  
	if (!is.null(soil_data)) {
		
		start <- index(point_data)[1]
		index(point_data) <- as.numeric(index(point_data)-start,units="secs")
		index(soil_data)  <- as.numeric(index(soil_data)-start,units="secs")
		point_data <- merge(point_data,soil_data)
		index(point_data) <- index(point_data)+start
		
		
		
	}


	} else {
		








		point_data <- soil_data
		
	}
  
	uvarp <- unique(c(ivarpoint,ivarsoil))
	unitpoint <- as.character(lookup_tbl_observation$unit[uvarp])
	names(unitpoint) <-  as.character(lookup_tbl_observation$obs_var[uvarp])
  
  
  
  
  
#  #print(parse_gnamespoint)
#  #evals_gnamespoint <- lapply(X=parse_gnamespoint,FUN=eval,envir=as.data.frame(point_data))
#  return(evals_gnamespoint)
#  ####eval_gnamespoint <- lapply(X=gnamesponit,FUN=function(x,pf){eval(parse(text=x),envir)},pf=as.data.frame(point_data))
#  
  
#  
#  names(onamespoint) <- gnamespoint
##  print(names(point_data))
##  print(gnamespoint)
##  print(names(point_data) %in% gnamespoint)
#  point_data <- point_data[,names(point_data) %in% gnamespoint]
#  nn <- onamespoint[names(point_data)]
#  names(point_data) <- nn
  
  
  ##### WRITE OUTPUT 
  
  nn <- names(point_data)
  names(nn) <- nn
 
  
  
  
  
 out <- NULL
	
  
  if (merge.output==TRUE) {
	  
	  start <- index(obs_data)[1]
	  index(obs_data) <- as.numeric(index(obs_data)-start,units="secs")
	  index(point_data) <- as.numeric(index(point_data)-start,units="secs")
	  names(obs_data) <- paste("OBS",names(obs_data),sep="_")
	  names(point_data) <- paste("SIM",names(point_data),sep="_")
	  #out <- list(observation=obs_data,simulation=point_data)
	  #out <- do.call(what=merge,args=out)
	  out <- merge(obs_data,point_data)
	  index(out) <- start+index(out)
	  
	  if(!is.null(when)) {
		  
		  iwhen <- which(index(out) %in% when)
		  
		  out <- out[iwhen,]
		  
		  
		  
	  }
	  
	  
	  ##INSERT ATTRIBUTE
	  attr(out,"observation_var") <- str_replace(names(obs_data),"OBS_","")
	  attr(out,"simulation_var") <- str_replace(names(point_data),"SIM_","")
	  attr(out,"var_unit") <- unitpoint
	#  iobsnames <- str_detect("OBS",names(alldata))
	#  isimnames <- str_detect("SIM",names(alldata))
	  
	#  obsnames <- names(alldata)[iobsnames]
	#  simnames <- names(alldata)[isimnames]
	  
	#  obsnames <- str_replace(obsnames,"OBS_","")
	#  simnames <- str_replace(simnames,"SIM_","")
	  
  ## TO GO ON .....
	  
	  
	  
  } else { 
  	  stop("Option not yet implemented, set merge.output=TRUE!")
	  out <- list(observation=obs_data,simulation=point_data)
  
  }
  
  
  return(out)
  
  
}