# TODO: Add comment
# 
# Author: ecor
###############################################################################
###
###
###  http://stackoverflow.com/questions/15595478/how-to-get-the-name-of-the-calling-function-inside-the-called-routine
###
rm(list=ls())

funx = function(...) {
	print(sys.call())
	print(sys.call(-1))
	callingFun = as.list(sys.call(-1))[[1]]
	calledFun = as.list(sys.call())[[1]]
	message(paste(callingFun, " is calling ", calledFun, sep=""))
}

funy = function(...) {funx(...)}

funy(a = 1, b = 2)
####funy is calling funx

