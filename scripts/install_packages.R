# TODO: Add comment
# 
# Author: lsalas
###############################################################################


libs<-c("jsonlite","plyr","dotenv")
kk<-try(install.packages(libs),silent=TRUE)
if(inherits(kk,"try-error")){
	stop(kk)
}
