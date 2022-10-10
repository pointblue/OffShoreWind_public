##################
## Author: Cotton Rockwood
## Date: Oct 10 2022
##
## Description: Code to set up a Fargate task programmatically in AWS using a
## Docker image and the `paws` package.
##
## Brief methods: 

#--------------------------------------------------------------------------
## Packages
libs <- c("paws")

lapply(libs, require, character.only = TRUE)

#--------------------------------------------------------------------------
# Docker image set-up
#--------------------------------------------------------------------------
# Create the Docker image (Inputs: R script(s) to be run in the Fargate task,
# any inputs to be coded as default environment variables to the container build,
# and the image definition)


#--------------------------------------------------------------------------
# Fargate set-up and registry
#--------------------------------------------------------------------------
# Initiate Elastic Container Registry (ECR) service
ecr_svc <- ecr()

#--------------------------------------------------------------------------
# Create AWS ECRrepository


#--------------------------------------------------------------------------
# Tag Docker image


#--------------------------------------------------------------------------
# Get ECR keys


#--------------------------------------------------------------------------
# Push image to ECR