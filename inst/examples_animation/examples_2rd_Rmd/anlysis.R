# TODO: Add comment
# 
# Author: Emanuele Cordano
###############################################################################
NULL
rm(list=ls())
library(geotopAnalytics)
library(dygraphs)
source('/home/ecor/Dropbox/R-packages/geotopAnalytics/R/extract.R') 

wpath <-  '/home/ecor/activity/2016/eurac2016/idra/B2_BeG_017_DVM_001_test_1' 
alldata <- GEOtop_ReadValidationData(wpath = wpath, save_rData = T)

obsnames <- attr(alldata,"observation_var")
simnames <- attr(alldata,"simulation_var")


iiv <- obsnames[7]
aiiv <- obsnames[1]
data <- extractGeotopVar(x=alldata,InputVar=iiv,Add_InputVar=aiiv,aggregate="monthly") 
datah <- extractGeotopVar(x=alldata,InputVar=iiv,Add_InputVar=aiiv,aggregate="hourly") 
unit <- attr(alldata,"var_unit")[iiv]
add_unit <- attr(alldata,"var_unit")[aiiv]
## 
dygraph(data, ylab=paste(iiv,"[",unit,"]",sep="")) %>%
		dyRangeSelector() %>%
		dyRoller() %>%
		dySeries(name = "additional.var", axis = "y2", stepPlot = TRUE, fillGraph = TRUE, label = paste(aiiv,"[",add_unit,"]",sep="")) %>%
		dyAxis(name="y2",label=paste("[",add_unit,"]",sep=""))







