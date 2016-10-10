#! /usr/bin/Rscript
# file vignette.R
#
# This file builds package vignette from Rmd format
#
# author: Emanuele Cordano on 16-01-2014
#
#This program is free software: you can redistribute it and/or modify
#it under the terms of the GNU General Public License as published by
#the Free Software Foundation, either version 3 of the License, or
#(at your option) any later version.
#
#This program is distributed in the hope that it will be useful,
#but WITHOUT ANY WARRANTY; without even the implied warranty of
#MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#GNU General Public License for more details.
#
#You should have received a copy of the GNU General Public License
#along with this program.  If not, see <http://www.gnu.org/licenses/>.

###############################################################################
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