simpath <- system.file("Muntatschini_pnt_1_225_B2_004",package="geotopOptim")
bin <-   "/home/ecor/local/geotop/GEOtop/bin/geotop-2.0.0"
runpath <- "/home/ecor/temp/geotopOptim_tests"

vars <- c("SoilAveragedTempProfileFile",	"SoilLiqWaterPressProfileFile",
		"SoilLiqContentProfileFile","SoilIceContentProfileFile","AvailableSoilWaterContent")


out <- geotopExec(bin=bin,simpath=simpath,runpath=runpath,
		clean=TRUE,getKeywords=vars,data.frame=TRUE,level=1)

param <- c(N=1.4,Alpha=0.0021,ThetaRes=0.05) 
out1 <- geotopExec(param=param,bin=bin,simpath=simpath,
		runpath=runpath,clean=TRUE,getKeywords=vars,
		data.frame=TRUE,level=1,intern=TRUE)


## Monthly-Aggregated Results: 

out2 <- geotopExec(param=param,bin=bin,simpath=simpath,
		runpath=runpath,clean=TRUE,getKeywords=vars,
		data.frame=TRUE,level=1,intern=TRUE,
		time.aggregate=list(FUN=mean,by="%Y-%m",na.rm=TRUE))



## SCALAR:: and VECTOR:: input params

param <- c(N=1.4,Alpha=0.0021,ThetaRes=0.05,SCALAR__LSAI=4.2)

out3 <- geotopExec(param=param,bin=bin,simpath=simpath,runpath=runpath,clean=TRUE,getKeywords=vars,data.frame=TRUE,level=1,intern=TRUE)


param <- c(N=1.4,Alpha=0.0021,ThetaRes=0.05,VECTOR__1__LSAI=4.2)

out4 <- geotopExec(param=param,bin=bin,simpath=simpath,runpath=runpath,clean=TRUE,getKeywords=vars,data.frame=TRUE,level=1,intern=TRUE)