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


#==========================================================================
# Fargate set-up and registry
#==========================================================================
# Initiate Elastic Container Registry (ECR) connection
# credentials and region should already be configured in the AWS CLI
ecr_svc <- ecr()

#--------------------------------------------------------------------------
# Create AWS ECR repository
ecr_svc$create_repository(repositoryName = "CoolPawsTest")

#--------------------------------------------------------------------------
# Tag Docker image
ecr_svc$tag_resource()

#--------------------------------------------------------------------------
# Get ECR keys
ecr_svc$get_authorization_token()

#--------------------------------------------------------------------------
# Push image to ECR
ecr_svc$initiate_layer_upload()

#==========================================================================
# Fargate Task registry
#==========================================================================
# Initiate Elastic Container Service (ECS) connection
ecs_svc <- ecs()

#--------------------------------------------------------------------------
# Create compute cluster
ecs_svc$create_cluster(clusterName, tags, settings, capacityProviders,
defaultCapacityProviderStrategy)

#--------------------------------------------------------------------------
# Register a task definition
ecs_svc$register_task_definition(
  family = "sleep360",
  taskRoleArn = "",
  neworkMode = "awsvpc",
  containerDefinitions = list(
    list(
      name = "sleep",
      command = list(
        "sleep",
        "360"
      ),
      cpu = 10L,
      essential = TRUE,
      image = "busybox",
      memory = 10L
    )
  ),
  volumes = list(),
  cpu = 256,
  memory = 512
)

  
#==========================================================================
# Fargate EBS creation and data transfer (May go in the Fargate Container code?)
#==========================================================================

#ec2_svc <- ec2()

#ec2_svc$create_volume(AvailabilityZone, Encrypted, Iops, KmsKeyId,
# OutpostArn, Size, SnapshotId, VolumeType, DryRun, TagSpecifications,
# MultiAttachEnabled, Throughput)

