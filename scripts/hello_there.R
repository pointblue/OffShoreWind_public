# TODO: Add comment
# 
# Author: lsalas
###############################################################################


## Preliminaries
libs<-c("dotenv","plyr","jsonlite")
kk<-suppressPackageStartupMessages(lapply(libs, require, character.only = TRUE))
namevar<-Sys.getenv("INPJSON")
nameval<-fromJSON(namevar)[[1]][1]
print(paste("Hello there",nameval))
