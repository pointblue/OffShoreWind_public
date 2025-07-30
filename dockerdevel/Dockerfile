## When building, use the --rm flag to force the container to remove itself when done

# Base image https://hub.docker.com/u/rocker/
FROM rocker/r-ver:4.2.0

RUN apt update \        
     apt install -y git
     
## Make the Workspace folder
RUN mkdir -p /Workspace

## clone the repo outside Workspace
RUN mkdir -p /repo
RUN git clone https://github.com/pointblue/OffShoreWind_public.git repo

## Install required R packages from script (which is in the repo too) - must include the package "dotenv"
RUN Rscript /repo/install_packages.R

### This goes in the R script...
## Mount the FSB or EBS
## data and output folders: we will map the EBS or FSB to these
## Need an environment variable that tells us where these are
## The drive mapping in the run --mount statement should be prefixes of outpth and datapth
## For example, if the output path is "/mnt/osw_files/output/", then
## OUTPATH is "output/", and the run statement is
##    docker run -it --rm --mount src:/mnt/osw_files/,target=/Workspace
## So OUTPATH tells the R session that inside Workspace there will be a folder called "output"
load_dot_env('.env')
outpth = paste0("~/Workspace/",as.character(Sys.getenv("OUTPATH")))
datapth = paste0("~/Workspace/",as.character(Sys.getenv("DATAPATH")))



CMD Rscript /repo/process2.R


## Update as follows: docker run --env INPJSON={\"nm\":\"Pepe\"} -ti --rm testp2cont
## Note no qoutes outside of curly brackets