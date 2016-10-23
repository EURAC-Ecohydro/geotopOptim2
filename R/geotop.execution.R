NULL 
#' Execution of GEOtop Hydrological Model
#' 
#' @param param vector of parameters to set for GEOtop simulation. Actually implemented only for soil type parameters. Deafault is \code{NULL}.
#' @param bin binary executable file of GEOtop with full path
#' @param inpts.file GEOtop configuretion \code{*.inpts} file. Default is \code{geotop.inpts}. 
#' @param simpath directory containg the \code{inpts.file}.
#' @param runpath directory where to run GEOtop
#' @param temporary.runpath logical value. If it \code{TRUE} GEOtop is running in a sub-directory of \code{runpath} made with randomly generated name.  Default is \code{FALSE}.
#' @param intern logical value, see \code{\link{system}}.
#' @param clean logical value. If it is \code{TRUE} previous simulations and other stuff in \code{runpath} were removed. See \code{\link{file.remove}} for functionality.
#' @param recovery logical value. If it is \code{TRUE}, GEOtop simulation are recovered or overwriiten according to the GEOtop settings in \code{inpts.file}. 
#' @param getKeywords character vector containing the keywords of \code{inpts.file} which can be imported after the GEOtop run. Default is \code{NULL}, nothing is imported.
#' @param data.frame logical vaue, see  \code{\link{get.geotop.inpts.keyword.value}}.  Default is \code{TRUE}, is enabled if \code{getKeywords} is not \code{NULL}.
#' @param date_field character vaue, see  \code{\link{get.geotop.inpts.keyword.value}}. Default is \code{"Date12.DDMMYYYYhhmm."}, is enabled if \code{getKeywords} is not \code{NULL}.
#' @param formatter character decimal formatter.  See \code{\link{get.geotop.inpts.keyword.value}}. Default is \code{"\%04d"}.
#' @param param.soil logical value If it \code{TRUE} (default) the vaues in \code{param} concerns soil parameters. 
#' @param paramPrefix character string. Default is \code{"Header"}. If \code{param.soil==TRUE}, the soil parameters in \code{param} are named with the corrosponding parameter keywords in \code{inpts.file} to which the  string \code{paramPrefix} is attached as a prefix. If \code{paramPrefix} is \code{NA} or \code{NULL} , the names of \code{param} elements must be matched with the corresponimg paramater name in the soil parameter data frame. 
#' @param SoilType  soil type used for GEOtop Calibration. This must be an ID of the soil types indicated in the \code{inpts.file} of The GEOtop Simulation. Default is 1.  If it is \code{NA} or \code{NULL}, the soil type is extracted from the soil map (\code{SoilMapFile}). 
#' @param level ID check point of a distributed or quasi-distributed GEOtop simulatiion where to perform the Point Calibration. Default is 1. See \code{\link{get.geotop.inpts.keyword.value}}
#' @param time.aggregate list of the arguments for \code{\link{aggregate.zoo}}. If it is \code{NULL} (default) no time aggragation is applied to the function outputs. Otherwise, the outputs are aggregated within time intervals. In case of monthly averaged values, it must  be set as \code{list(FUN=mean,by="\%Y-\%m",na.rm=TRUE)} where the \code{by} item is the string format according to POSIX standard for times and dates represented the aggregation interval ( sse \code{\link{strptime}}. 
#' @param names_par OPTIONAL vector of names for \code{param}
#' @param ... further arguments for \code{\link{get.geotop.inpts.keyword.value}}
#' 
#' 
#' @details In this function, implementation, The parameters entered through \code{param} replace  only the ones of  the first (\code{"0001"}) soil type  declared in the GEOtop simulation directory, if \code{SoilType} argument is omitted. The elements of the vector \code{param} must be named with the respective keywords preceded by the prefix \code{SOIL__} if the parameter is a soil property through \code{SoilParFIle} file (see package documentation); \code{SCALAR__} if the parameter is scalar (one numeric value) and assigned directly in the \code{inpts.file} file (e.g. \code{"geotop.inpts"}); \code{VECTOR__[X]__} if the parameter is assigned in the \code{inpts.file} file and is the \code{X}-th element of a vector (e.g. assignments of land use proprties). 
#' 
#' @export
#' 
#' @importFrom stringr str_split str_replace_all str_sub str_trim
#' @importFrom utils read.table write.table
#' 
#' @importFrom geotopbricks get.geotop.inpts.keyword.value
#' @importFrom soilwater swc
#' @importFrom zoo index<- index as.zoo
#' 
#' @examples
#' 
#' simpath <- system.file('geotop-simulation/B2site',package="geotopOptim2")  #######system.file("Muntatschini_pnt_1_225_B2_004",package="geotopOptim")
#' bin <-   "/home/ecor/local/geotop/GEOtop/bin/geotop-2.0.0"
#' runpath <- "/home/ecor/temp/geotopOptim_tests"
#' 
#' vars <- c("SoilAveragedTempProfileFile",	"SoilLiqWaterPressProfileFile",
#' "SoilLiqContentProfileFile","SoilIceContentProfileFile",
#' "AvailableSoilWaterContent","PointOutputFile")
#' 
#' 
#' out <- geotopExec(bin=bin,simpath=simpath,runpath=runpath,
#'       clean=TRUE,getKeywords=vars,data.frame=TRUE,level=1)
#' 
#' param <- c(N=1.4,Alpha=0.0021,ThetaRes=0.05) 
#' out1 <- geotopExec(param=param,bin=bin,simpath=simpath,
#' 			runpath=runpath,clean=TRUE,getKeywords=vars,
#' 			data.frame=TRUE,level=1,intern=TRUE)
#' 
#' 
#'  ## Monthly-Aggregated Results: 
#' 
#'  out2 <- geotopExec(param=param,bin=bin,simpath=simpath,
#' 			runpath=runpath,clean=TRUE,getKeywords=vars,
#' 			data.frame=TRUE,level=1,intern=TRUE,
#' 			time.aggregate=list(FUN=mean,by="%Y-%m",na.rm=TRUE))
#' 
#' 
#' 
#'  ## SCALAR__ and VECTOR__ input params
#' 
#' param <- c(N=1.4,Alpha=0.0021,ThetaRes=0.05,SCALAR__LSAI=4.2)
#' 
#' out3 <- geotopExec(param=param,bin=bin,simpath=simpath,
#' 			runpath=runpath,clean=TRUE,getKeywords=vars,
#' 			data.frame=TRUE,level=1,intern=TRUE)
#' 
#' 
#' param <- c(N=1.4,Alpha=0.0021,ThetaRes=0.05,VECTOR__1__LSAI=4.2)
#' 
#' out4 <- geotopExec(param=param,bin=bin,simpath=simpath,
#' 			runpath=runpath,clean=TRUE,getKeywords=vars,
#' 			data.frame=TRUE,level=1,intern=TRUE)
#' 
#' 
#' 
#' 
#' 
#
#PointOutputFile	=	"tabs/point"
#PointAll	=	1
#
#!SnowProfileFile	=	"tabs/snow"
#SnowDepthLayersFile	="tabs/snowDepth"
#SnowTempProfileFile	=	"tabs/snowT"
#SnowLiqContentProfileFile	=	"tabs/snowLiq"
#SnowIceContentProfileFile	=	"tabs/snowIce"
#SnowAll	=	1
#
#BasinOutputFile	=	"tabs/basin"
#BasinAll	=	1
#
#SoilAveragedTempProfileFile		=	"tabs/soilTz"
#SoilLiqWaterPressProfileFile	=	"tabs/psiz"
#SoilLiqContentProfileFile		=	"tabs/thetaliq"
#SoilIceContentProfileFile		=	"tabs/thetaice"
#SoilAll=1

#
#
#
#
#





geotopExec <- function (param=NULL,bin="/home/ecor/local/geotop/GEOtop/bin/geotop-2.0.0",simpath,inpts.file="geotop.inpts",
		runpath="/home/ecor/temp/geotopOptim_tests",temporary.runpath=FALSE,clean=TRUE,recovery=!clean,getKeywords=NULL,
		data.frame=TRUE,date_field="Date12.DDMMYYYYhhmm.",intern=FALSE,param.soil=TRUE,formatter = "%04d",paramPrefix="Header",names_par=NULL,SoilType=1,level=1,time.aggregate=NULL,...) {
	

	
	
	message("Preparing a GEOtop Simulation!!")
	msg <- sprintf("wpath:%s at %s",simpath,Sys.time())
	message(msg)
	
	t <- str_split(simpath,"/")[[1]]
	simdir <- t[length(t)]
	
##	rundir <- paste(runpath,simdir,sep="/")
	## EC 20150609
	
	if (is.null(temporary.runpath)) temporary.runpath <- FALSE
	if (is.na(temporary.runpath)) temporary.runpath <- FALSE
	if (temporary.runpath==TRUE) {
		
		## EC 20150611
		    runpath0 <- paste(runpath,simdir,sep="/")
			#runpath0 <- runpath
			
			 
			 repeat {
				 
				#runstring <- c(letters,0:9)
				#runstring <- sample(c(sample(runstring),sample(runstring),sample(runstring),sample(runstring)))
			    #nchar <- 20
				#runstring <- paste(runstring[1:nchar],collapse="")
						 
			
				#runpath <- paste(runpath0,runstring,sep="/")
				
				runpath <- tempfile(pattern = "xx", tmpdir = runpath0,fileext = "")
			
				if (file.exists(runpath)!=TRUE) {
					##print(param)
					##print(sprintf("Create %s at %s",runpath,Sys.time()))
					break
				}
				
				
			
			
			} 
			
			if (file.exists(runpath)==TRUE) {
				msg <- paste(runpath," already exists!!",sep=" ")
			
				stop(msg)
			}
			
			## EC 20150611
			
			dir.create(runpath,recursive=TRUE)
			
			rundir <- paste(runpath,simdir,sep="/")
			
		
	} else {
		
		rundir <- paste(runpath,simdir,sep="/")
		
		
	}
	
	## EC 20150609
	
	
	
	if (recovery==TRUE) clean <- FALSE
	
	
	if (clean==TRUE) {
		
		torv <- list.files(rundir,full.names=TRUE,include.dirs=TRUE,recursive=TRUE)
		file.remove(torv)
		torv <- list.files(rundir,full.names=TRUE,include.dirs=TRUE,recursive=TRUE)
		file.remove(torv)
		
	}
	
	file.copy(from=simpath,to=runpath,recursive=TRUE,overwrite=TRUE)
	
	## MOdify input file according to param:
	
	
	#print(param)
	#print(names(param))
	#print(param.soil)
	#print(names_par)
	##print("rundir:")
	##print(rundir)
	##message(as.character(SoilType))
	if (is.null(SoilType)) SoilType <- NA
	if (is.na(SoilType)) {
		
		
		SoilType <- extract.geotop.value.fromMap("SoilMapFile",wpath=rundir,inpts.file=inpts.file)[level,"SoilMapFile"]

		
	}
	
	
	
	
	
	if (!is.null(param)) {
		
		if (is.null(names(param))) {
			
			names(param) <- names_par
			
		}
		
		if (is.null(names(param))) {
			
			warning("param has NO NAMES and will be ignored!")
			
		} else if (param.soil==TRUE) {
			
			ScalarPrefix <- "SCALAR__"
			VectorPrefix <- "VECTOR__"
			IntegerPrefix <- "INTEGER__"
###			print(any(str_detect(names(param),ScalarPrefix)))
			
			
			if (any(str_detect(names(param),ScalarPrefix))) {
				
				
			            #message('param with SCALAR')
						#message(param)
						##### CHECK Scalar Keywords 
						
						## wanrnig: Do not put comments with ! in the same row of the keyword called with SCALAR__...
						inpts.path <- paste(rundir,inpts.file,sep="/")
						inpts.vv   <- readLines(inpts.path)
						inpts.list <- str_split(inpts.vv,"=")
						kws <- sapply(X=inpts.list,FUN=function(x){str_trim(x[[1]])})
						
						n_params <- str_replace(names(param),ScalarPrefix,"")
						names(n_params) <- names(param)
						
						ikws <- which(kws %in% n_params) 
						
					#	print(n_params)
						
						
					#	print(kws[ikws])
		                scalarnames <- paste(ScalarPrefix,kws[ikws],sep="")
						inpts.vv[ikws] <- paste(kws[ikws],param[scalarnames],sep="=")
						
						
						writeLines(inpts.vv,con=inpts.path)
						
						param <- param[!(names(param) %in% scalarnames)]
						
						
					##	print(inpts.vv)
					
						
						## .....
						
				
			}
			
			if (any(str_detect(names(param),IntegerPrefix))) {
				
				
				message('param with INTEGER')
				message(param)
				##### CHECK Scalar Keywords 
				
				## wanrnig: Do not put comments with ! in the same row of the keyword called with SCALAR__...
				inpts.path <- paste(rundir,inpts.file,sep="/")
				inpts.vv   <- readLines(inpts.path)
				inpts.list <- str_split(inpts.vv,"=")
				kws <- sapply(X=inpts.list,FUN=function(x){str_trim(x[[1]])})
				
				n_params <- str_replace(names(param),IntegerPrefix,"")
				names(n_params) <- names(param)
				
				ikws <- which(kws %in% n_params) 
				
				#	print(n_params)
				
				
				#	print(kws[ikws])
				integernames <- paste(IntegerPrefix,kws[ikws],sep="")
				inpts.vv[ikws] <- paste(kws[ikws],param[integernames],sep="=")
								
				writeLines(inpts.vv,con=inpts.path)
				
				
				stop("THIS IS STILL TO DO!!!!")
				param <- param[!(names(param) %in% integernames)]
				
				
				##	print(inpts.vv)
				
				
				## .....
				
				
			}
			
			
			
			
			
			if (any(str_detect(names(param),"VECTOR__"))) {
				
				
				
				##### CHECK VECTOR Keywords 
				
				
				## SYNTAX: VECTOR__1__<keyword>
				
				######
				
				nn_params <- names(param)[str_detect(names(param),VectorPrefix)]
				
				
				o_params <- lapply(X=nn_params,FUN=function(x,param) {
							
							xold <- x
							x <- str_split(xold,"__")[[1]]
							
							if (length(x)!=3) {
								msg <- paste(x,collapse="__")
								msg <- sprintf("VECTOR INPUT Keyword badly defined: %s",msg)
								stop(msg)
							}
							
							
							names(x) <- c("TYPE","INDEX","VARIABLE")
							
							x["PARAM"] <- xold
							
							x <- as.data.frame(t(x),stringsAsFactors=FALSE)
							
							####
							#  #
							#  #
							####
							x[,"INDEX"] <- as.integer(x[,"INDEX"])
							x[,"VALUE"] <- param[xold]
							return(x)
							
							
							
						},param=param)
			##	o_params 
				o_params <- do.call(cbind,o_params)
				n_params <- unique(o_params$VARIABLE)
				
				value_params <- lapply(X=n_params,FUN=get.geotop.inpts.keyword.value,wpath=rundir,inpts.file=inpts.file,numeric=TRUE,vector_sep=",")
				names(value_params) <- n_params
				
				#str(o_params)
				
				#str(value_params)
				
				for (i in 1:nrow(o_params)) {
					
						tmpval <- value_params[[o_params$VARIABLE[i]]]
						tmpval[o_params$INDEX[i]] <- o_params$VALUE[i]
						value_params[[o_params$VARIABLE[i]]] <- tmpval
						
				}	
				
				
				value_params <- lapply(X=value_params,FUN=paste,collapse=",")
				#str(o_params)
				
				#str(value_params)
				
			
	
				## wanrnig: Do not put comments with ! in the same row of the keyword called with SCALAR__...
				
				inpts.path <- paste(rundir,inpts.file,sep="/")
				inpts.vv   <- readLines(inpts.path)
				inpts.list <- str_split(inpts.vv,"=")
				kws <- sapply(X=inpts.list,FUN=function(x){str_trim(x[[1]])})
				
				ikws <- which(kws %in% names(value_params)) 
				
				
				
				inpts.vv[ikws] <- paste(kws[ikws],value_params[kws[ikws]],sep="=")
				
				
				writeLines(inpts.vv,con=inpts.path)
				
				vectornames <- paste(VectorPrefix,kws[ikws],sep="")
				param <- param[!(str_detect(names(param),VectorPrefix))]
				## ..... 
				
				## .....
				
				
			}
			##### INSERT SCALAR VALUE 
			
			
			
			
			
			
			
			
			
			
			
			####
#			HeaderLateralHydrConductivity	=	"Kh"
#			HeaderNormalHydrConductivity	=	"Kv"
#			HeaderThetaRes	=	"vwc_r"
#			HeaderWiltingPoint	=	"vwc_w"
#			HeaderFieldCapacity	=	"vwc_fc"
#			HeaderThetaSat	=	"vwc_s"
#			HeaderAlpha	=	"alpha"
#			HeaderN	=	"n"
#			HeaderSpecificStorativity	=	"stor"
#			HeaderKthSoilSolids	=	"Kth"
#			HeaderCthSoilSolids	=	"Cth"
			
			## 
			
			
			
			SoilPrefix <- "SOIL__"
			if (!all(str_detect(names(param),SoilPrefix))) {
				
				
				xxxx <- paste(names(param),sep=";")
				msg <- sprintf("Add SOIL__ prefix for the keywords referring to soil properties: %s !!!",xxxx)
				
				warning(msq)
			}
			names(param) <- str_replace(names(param),SoilPrefix,"")
			
			
			variable.soil.depth <- FALSE
			if (all(c("SoilDepth","NumberOfSoilLayers") %in% names(param))) {
				
				SoilDepth <- as.numeric(param["SoilDepth"])
				SurfaceSoilLayer <- as.numeric(param["SurfaceSoilLayer"])
				
				NumberOfSoilLayers  <- ceiling(as.numeric(param["NumberOfSoilLayers"]))
				NumberOfSoilLayers[NumberOfSoilLayers<4] <- 4
				
				
				variable.soil.depth <- TRUE
				param <- param[!(names(param) %in% c("SoilDepth","NumberOfSoilLayers","SurfaceSoilLayer"))]
			}
		
		
			if (c("PsiGamma") %in% names(param)) {
			
				psiGamma <- as.numeric(param["PsiGamma"])	
				
				
				
			}	else {
				
				psiGamma <- NA
				
			}
			
			######
			param <- param[!(names(param) %in% c("PsiGamma"))]
			### INSERT BOTTOM LAYER!!
			
			param_bottomlayer <- param[str_detect(names(param),"_bottomlayer")]
			param <- param[!(param %in% param_bottomlayer)]
			names(param_bottomlayer) <- str_replace_all(names(param_bottomlayer),"_bottomlayer","")
			
			
		    if (c("SoilInitPresL0001") %in% names(param))	{
				
				parSoilInitPresL <- which(str_detect(names(param),"SoilInitPresL"))
				param_soil <- param[parSoilInitPresL]
				param <- param[-parSoilInitPresL]
				len <- length(param_soil)
				
				names_param_soil <- sprintf("SoilInitPresL%04d",1:len)
				psisoil <- param_soil[names_param_soil]
				
				
				
			}	else {
				
				psisoil <- NULL
			}	
			
			if (any(str_detect(names(param),"_V_"))) {
				
				
				iparamLs <- which(str_detect(names(param),"_V_"))
				
				
			    paramLs <- param[iparamLs]
				
				nparamLs <- str_sub(names(paramLs),end=-9) ## "_V_L0001" 
				param <- param[-iparamLs]
				
				
				
				
				
			} else {
				
				paramLs <- NULL
				nparamLs <- NULL
				
			}	
				
			
			
			
				
				
				
			###	nparamLs <- names(paramLs)
				
				
				
				
##				uparamLs <- unique(paramls)
				
				
				
				
				
		#		param_df <- array(NA,c(max(layers),length(uparams)))
		#		names(param_df) <- 
						
				## CONTINUE HERE 
				## CONTINUE HERE 
				
				
		#		paramL <- TRUE
				
				
				
		#	} else {
				
			#	paramL <- FALSE
		#	}
			
			
			
			param.soil.df.filename <- get.geotop.inpts.keyword.value("SoilParFile",wpath=rundir,inpts.file=inpts.file,add_wpath=TRUE)
			param.soil.df.filename <- paste(param.soil.df.filename,formatter,".txt",sep="")
##			layer <- 1 
			param.soil.df.filename <- sprintf(param.soil.df.filename,SoilType)
			
			param.soil.df <- read.table(param.soil.df.filename,header=TRUE,sep=",")
			
			
			if (is.null(paramPrefix)) ParamPrefix <- NA
			if (!is.na(paramPrefix)) {
				ids <- paste(paramPrefix,names(param),sep="")
				names(param) <- get.geotop.inpts.keyword.value(ids,wpath=rundir,inpts.file=inpts.file)
				### check Initial Condition
				if (length(param_bottomlayer)>0) {
					
					ids_b <- paste(paramPrefix,names(param_bottomlayer),sep="")
					names(param_bottomlayer) <- get.geotop.inpts.keyword.value(ids_b,wpath=rundir,inpts.file=inpts.file)
					
				}
				if (length(nparamLs)>0) {
					
					#print(nparamLs)
					ids_lb <- paste(paramPrefix,nparamLs,sep="")
					nparamLs <- unlist(get.geotop.inpts.keyword.value(ids_lb,wpath=rundir,inpts.file=inpts.file))
					#print(nparamLs)
					
					
				}
				
				WiltingPoint <- get.geotop.inpts.keyword.value(paste(paramPrefix,"WiltingPoint",sep=""),wpath=rundir,inpts.file=inpts.file)
				FieldCapacity <- get.geotop.inpts.keyword.value(paste(paramPrefix,"FieldCapacity",sep=""),wpath=rundir,inpts.file=inpts.file)
				ThetaSat <- get.geotop.inpts.keyword.value(paste(paramPrefix,"ThetaSat",sep=""),wpath=rundir,inpts.file=inpts.file)
				ThetaRes <- get.geotop.inpts.keyword.value(paste(paramPrefix,"ThetaRes",sep=""),wpath=rundir,inpts.file=inpts.file)
				VG_Alpha <- get.geotop.inpts.keyword.value(paste(paramPrefix,"Alpha",sep=""),wpath=rundir,inpts.file=inpts.file)
				VG_N <- get.geotop.inpts.keyword.value(paste(paramPrefix,"N",sep=""),wpath=rundir,inpts.file=inpts.file)
				Dz <- get.geotop.inpts.keyword.value(paste(paramPrefix,"SoilDz",sep=""),wpath=rundir,inpts.file=inpts.file)
				SoilInitPres <- get.geotop.inpts.keyword.value(paste(paramPrefix,"SoilInitPres",sep=""),wpath=rundir,inpts.file=inpts.file)
			
				
			}
			
			
			
			
			if (variable.soil.depth==TRUE) {
				
				
				
				if(is.na(SurfaceSoilLayer)) SurfaceSoilLayer <- param.soil.df[1,Dz]
				
				
				param.soil.df <- param.soil.df[1:NumberOfSoilLayers,]
				
				param.soil.df[1:NumberOfSoilLayers,] <- param.soil.df[1,]
				
				polycoeff <- array(1,NumberOfSoilLayers)
				polycoeff[1] <- 1-SoilDepth/SurfaceSoilLayer
				
				lambda <- polyroot(polycoeff)
				lambda <- lambda[Re(lambda)>=0]
				
				ail <- abs(Im(lambda))
				
			
				lambda <- Re(lambda[which.min(ail)])
				lambda <- lambda[lambda>=0][1]    ## Get positive or null solution!!
				param.soil.df[,Dz] <- SurfaceSoilLayer*lambda^(0:(NumberOfSoilLayers-1))
				
				### TO TEST!!!
				
				
				
			}
			
		# INSERT HERE 
			if (!is.null(paramLs)) {
				
			#	print(paramLs)
			#	print("xx")
			#	print(names(param.soil.df))
				
				
				
				layers <- as.numeric(str_sub(names(paramLs),-4))
				Lscond <- which((nparamLs %in% names(param.soil.df)) & (layers<=nrow(param.soil.df)))
			#	print(nparamLs)
			#	print("yy")
				paramLs <- paramLs[Lscond]
				nparamLs <- nparamLs[Lscond]
				layers <- layers[Lscond]
				
				names(layers) <- names(paramLs)
				
				
			#	print(paramLs)
				for (ii in 1:length(paramLs)) {
				
						it <- nparamLs[ii]
						lz <- layers[ii]
#						print(it)
#						print(lz)
#						print(paramLs[ii])
						param.soil.df[lz,it] <- paramLs[ii]
				
				
				}
			}
				
				
				
				
		      
		
			
			
			### PUT VARIABLES HERE
			
			
			
			
			### Adjust bottom layer 
			
			dz <- param.soil.df[,Dz]
			z <- dz/2
			for(i in 2:length(z)) {
				z[i] <- z[i-1]+dz[i]/2+dz[i-1]/2
				
			}
			
			zm <- (z-z[1])/(z[length(z)]-z[1])
			
			for (it in names(param)) {
				
				param.soil.df[,it] <- param[[it]]
				
				
				if (it %in% names(param_bottomlayer)) {
					
					#### DO INTERPOLATION 
					
					param.soil.df[,it] <- param[[it]]^(1-zm)*param_bottomlayer[[it]]^zm
					   
				}	
				
			}
		
			
			
			if (!is.na(psiGamma)) {
#				print(Dz)
#				print(param.soil.df)
				
				
				
				
				if (SoilInitPres %in% names(param)) {
					
					param.soil.df[,SoilInitPres] <- param[[SoilInitPres]]+psiGamma*z
				}
				
				if (!is.null(psisoil)) {
					
					param.soil.df[,SoilInitPres][1:length(psisoil)] <- psisoil
					
					zfront <- z[length(psisoil)]
					
					param.soil.df[,SoilInitPres][z>zfront] <- psisoil[length(psisoil)]+psiGamma*(z[z>zfront]-zfront)
			
					
				}	
				
				
				
				
				
			}
			
			
			### CORRECT SOIL WATER
			alpha <-  param.soil.df[,VG_Alpha]
			n <- param.soil.df[,VG_N]
			theta_sat <- param.soil.df[,ThetaSat]
			theta_res <- param.soil.df[,ThetaRes]
		###	=alpha,n=n,theta_sat=theta_sat,theta_res=theta_res)
			waterdensity <- 1000 ## kg/m^3
			gravity <- 9.81 ## m/s^2
			
			
			psi_WP <- -1500*1000 ## Pa
			psi_FC <- -33*1000  ##  Pa
			
			psi_WP <- psi_WP/(waterdensity*gravity)*1000 ## converted to water millimiters according to GEOtop
			psi_FC <- psi_FC/(waterdensity*gravity)*1000 ## converted to water millimiters according to GEOtop
			
			param.soil.df[,FieldCapacity] <- swc(psi=psi_FC,alpha=alpha,n=n,theta_sat=theta_sat,theta_res=theta_res,type_swc="VanGenuchten")
			param.soil.df[,WiltingPoint] <- swc(psi=psi_WP,alpha=alpha,n=n,theta_sat=theta_sat,theta_res=theta_res,type_swc="VanGenuchten")
			###
#			print(param.soil.df)
			write.table(x=param.soil.df,file=param.soil.df.filename,sep=",",quote=FALSE,row.names = FALSE,col.names = TRUE)
		
			
			
			
	####		print("WRITTEN GEOTOP SOIL PARAM FILE")
			
			
		} else {
			
			warning("param.soil is not TRUE: no other methods are implemented, param will be ignored")
			param <- NULL
		}
		
		
		
	}
	message("GEOtop is running!!")
	msg <- sprintf("rundir:%s at %s",rundir,Sys.time())
	message(msg)
	command.line <- paste(bin,rundir,sep=" ")
	
	cc <- system(command.line,intern=intern)
	######cca <- cc
	msg <- sprintf("FINISH rundir:%s at %s",rundir,Sys.time())
	message(msg)
 
	
	
	
	
	
	if (length(getKeywords)>0) {
		
		
		getKeywords00 <- getKeywords
		###  Set Particlar Keywords: 
		
		### SET AVAILABLE WATER CONTENT (AGRO-METEOROLOGY) 
		use_AvailableSWC_keyword <- "AvailableSoilWaterContent"
		if (c(use_AvailableSWC_keyword) %in% getKeywords) {
			
			
			
			getKeywords <- unique(c(getKeywords,"SoilLiqContentProfileFile"))
			getKeywords <- getKeywords[!(getKeywords %in% c(use_AvailableSWC_keyword))]
			use_AvailableSWC = TRUE
			
			
		} else {
			
			use_AvailableSWC = FALSE
		}
			
			
			
			
			
	    ### END Set Particular Keywords
		
		
		
		
		
	
		out <- lapply(X=getKeywords,FUN=get.geotop.inpts.keyword.value,wpath=rundir,inpts.file=inpts.file,data.frame=data.frame,date_field=date_field,formatter=formatter,level=level,...)
		names(out) <- getKeywords
		
		if (use_AvailableSWC==TRUE) {
			
			## Reread the soil parameters file
	
			
			out[[use_AvailableSWC_keyword]] <- NULL 
			
			param.soil.df.filename <- get.geotop.inpts.keyword.value("SoilParFile",wpath=rundir,inpts.file=inpts.file,add_wpath=TRUE)
			param.soil.df.filename <- paste(param.soil.df.filename,formatter,".txt",sep="")
#			layer <- 1 
			param.soil.df.filename <- sprintf(param.soil.df.filename,SoilType)
			param.soil.df <- read.table(param.soil.df.filename,header=TRUE,sep=",")
			## wilting point label
			
			WiltingPoint <- get.geotop.inpts.keyword.value(paste(paramPrefix,"WiltingPoint",sep=""),wpath=rundir,inpts.file=inpts.file)
			
		
			out[[use_AvailableSWC_keyword]] <- out[["SoilLiqContentProfileFile"]]
			
			zcol <- which(str_detect(names(out[[use_AvailableSWC_keyword]]),"X"))
			
			for (iz in 1:length(zcol)) {
				##message(param.soil.df[iz,WiltingPoint])
				awc <- as.vector(out[["SoilLiqContentProfileFile"]][,zcol[iz]]-param.soil.df[iz,WiltingPoint])
				awc[awc<0.0] <- 0.0
				out[[use_AvailableSWC_keyword]][,zcol[iz]] <- awc
			    
#				if (out[[use_AvailableSWC_keyword]][,zcol[iz]]==out[["SoilLiqContentProfileFile"]][,zcol[iz]]) {
#					
#					stop(iz)
#				}
			}
			
			
			
			#str(param.soil.df) 
			
			
		}
		
### TEMPORAL AGGREGATION 
		
	if (!is.null(time.aggregate)) {
		
		out <- lapply(X=out,FUN=function(x,time.aggregate) {
					
					if ((class(time.aggregate[["by"]])=="character") & (length(time.aggregate[["by"]])==1))   {
						time.aggregate[["by"]] <- as.character(index(x),format=time.aggregate[["by"]])
					}
					time.aggregate[["x"]]		<- x 
					out <- do.call(what="aggregate",args=time.aggregate)
					
				},time.aggregate=time.aggregate)
	
		
		
		if (temporary.runpath==TRUE) {
			
			unlink(runpath,recursive=TRUE)
			
			
		}
		
		
		
		
	}



		
		
	} else {
		
		out <- rundir
		
	}
	
	
	
	
	attr(out,"output_execution") <- cc
	message("End GEOtop Running!")
	return(out)
	
}