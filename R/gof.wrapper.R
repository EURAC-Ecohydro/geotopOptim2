# TODO: Add comment
# 
# Author: ecor
###############################################################################
#' 
#' Wrapper function for hydroGOF::gof function 
#' 
#' @param sim,obs,... argument for \code{\link{gof}}
#' 
#' @export
#' @importFrom hydroGOF gof
#' @details This functio is a wrapper of \code{\link{gof}}. In case \code{obs} or \code{sim} are all \code{NA}, it returns a matrix of \code{NA}.
#' 
#' 
#' @seealso \code{\link{gof}}
#' 

gofg <- function(sim,obs,...) {
	
	
	cond <- all(is.na(sim)) | all(is.na(obs))
	if (cond==TRUE) {
		
		 out <- gof(1:10,1:10,...)
		 out[,] <- NA
		
	} else {
		
		
		out <- gof(sim=sim,obs=obs,...)
	}
	
	return(out)
	
}
