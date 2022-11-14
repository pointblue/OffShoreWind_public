##################
## Author: Cotton Rockwood
## Date: Nov 10 2022
##
## Description: Code to set up an EFS volume in AWS using the `paws` package
## and copy the data necessary for Process 2 from the S3 bucket to the EFS.
##
## Brief methods: 

#--------------------------------------------------------------------------
## Packages
libs <- c("paws")

sapply(libs, require, character.only = TRUE)

#==========================================================================
# EFS set-up
#==========================================================================

#--------------------------------------------------------------------------
# Initiate Elastic Container Registry (ECR) connection
# credentials and region should already be configured in the AWS CLI
efs_svc <- efs()

#--------------------------------------------------------------------------
# Create AWS ECR repository
efs_info <- efs_svc$create_file_system(
  CreationToken = "Process2_FS",
  PerformanceMode = "generalPurpose",
  Tags = list(
    list(
      Key = "Name",
      Value = "Process2_EFS"
    )
  )
)

FS_Id <- efs_info$FileSystemId

# pause execution until file system is created
while(!efs_svc$describe_file_systems(
  FileSystemId = FS_Id)$FileSystems[[1]]$LifeCycleState == "available") {
  Sys.sleep(0.1)
}


# Create mount target
FS_mount <- efs_svc$create_mount_target(FileSystemId = FS_Id,
                            SubnetId = "subnet-0aa63ed59d520cb0c")

#==========================================================================
# File copying is done with manually created DataSync task
#==========================================================================