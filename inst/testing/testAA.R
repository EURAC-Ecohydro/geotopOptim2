# TODO: Add comment
# 
# Author: ecor
###############################################################################


wpath <-  "/home/ecor/temp/geotopOptim_tests/B2_BeG_017_DVM_001_test_1/0236p9jbux2wczueto7g/B2_BeG_017_DVM_001_test_1"
bin <-  "/home/ecor/local/geotop/GEOtop/bin/geotop-2.0.0"


command <- paste(bin,wpath,sep=" ")
		
o <- system(command)

wpath2 <- '/home/ecor/temp/geotopOptim_tests/B2_BeG_017_DVM_001_test_1/2rohgc761mum8n3r7648/B2_BeG_017_DVM_001_test_1'


command2 <- paste(bin,wpath2,sep=" ")
o2 <- system(command2)

