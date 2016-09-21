rm(list=ls())


library(rmarkdown)

path <- "/home/ecor/Dropbox/R-packages" 
verbose=TRUE

pkg_name <- "geotopOptim2"
dir <- paste(path,pkg_name,"vignettes",sep="/")

rmds <- list.files(dir,pattern=".Rmd",full.name=TRUE)

for (it in rmds) {
	
	if (verbose) {
		msg <- sprintf("Transforming %s",it)
		message(msg)
	}
	
	render(it)
	### https://www.bioconductor.org/help/course-materials/2015/CSAMA2015/lab/rr-authoring.html#from-rmd-to-html-or-pdf
}