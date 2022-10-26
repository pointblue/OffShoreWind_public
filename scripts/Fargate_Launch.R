##################
## Author: Cotton Rockwood
## Date: Oct 10 2022
##
## Description: Code to run Leo's "test-greet" Fargate task programmatically 
## in AWS using the `paws` package.
##
## Brief methods: 

#--------------------------------------------------------------------------
## Packages
libs <- c("paws")

lapply(libs, require, character.only = TRUE)

#==========================================================================
# Run Fargate task
#==========================================================================
# Initiate Elastic Container Service (ECS) connection
# credentials and region should already be configured in the AWS CLI
ecs_svc <- ecs(config = list(
  region = "us-east-2"
  )
)

#--------------------------------------------------------------------------
# start the Fargate task
ecs_svc$run_task(cluster = "test-greet-cluster", # name or ARN of cluster
                 count = 1, # number the specified task to instantiate on your cluster (<= 10)
                 launchType = "FARGATE", # 'EC2'|'FARGATE'
                 platformVersion = "LATEST",
                 taskDefinition = "test-greet-task",
                 networkConfiguration = list( # see https://docs.aws.amazon.com/AmazonECS/latest/userguide/fargate-task-networking.html
                   awsvpcConfiguration = list(
                     subnets = list(
                       "subnet-0e86140db2cd142ae"
                     ),
                     assignPublicIp = "ENABLED"
                   )),
                   
                   overrides = list(
                     containerOverrides = list(
                       list(name = "test-greet",
                            environment = list(
                         list(
                           name = "HI_TO",
                           value = "Cotton"
                         )
                       )))))
                 #       environmentFiles = list(
                 #         list(
                 #           value = "string",
                 #           type = "s3"
                 #         )
                 #       ),
                 #       cpu = 123,
                 #       memory = 123,
                 #       memoryReservation = 123,
                 #       resourceRequirements = list(
                 #         list(
                 #           value = "string",
                 #           type = "GPU"|"InferenceAccelerator"
                 #         )
                 #       )
                 #     )
                 #   ),
                 #   cpu = "string",
                 #   inferenceAcceleratorOverrides = list(
                 #     list(
                 #       deviceName = "string",
                 #       deviceType = "string"
                 #     )
                 #   ),
                 #   executionRoleArn = "string",
                 #   memory = "string",
                 #   taskRoleArn = "string"
                 # ),
                 # capacityProviderStrategy = list( # uses default for cluster if not provided; must be omitted if a launchType is specified
                 #                                 list(
                 #                                   capacityProvider = "string", # 'FARGATE'|'FARGATE_SPOT'
                 #                                   weight = 123,
                 #                                   base = 123
                 #                                 )
                 # ),
                 # enableECSManagedTags =, # T|F; see https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-using-tags.html
                 # propagateTags, #'TASK_DEFINITION'|'SERVICE'; Specifies whether to propagate the tags from the task definition to the task
                 # tags = list(
                 #   list(
                 #     key = "string",
                 #     value = "string"
                 #   )
                 # ),
                 # group = "string", # name of the task group to associate with the task
                 # placementConstraints = list(
                 #   list(
                 #     type = "distinctInstance"|"memberOf",
                 #     expression = "string"
                 #   )
                 # ),
                 # placementStrategy = list(
                 #   list(
                 #     type = "random"|"spread"|"binpack",
                 #     field = "string"
                 #   )
                 # ),
                 # referenceId = "string", # the reference ID to use for the task
                 # startedBy = "string", # An optional tag specified at task start. E.g., a unique identifier for an automatically triggered task to run a batch process
                 

# #---------------------------------------------
# # Clean up - things we may wish to clean up
# 
# # Deregister task
# ecs_svc$deregister_task_definition(taskDefinition = "string")
# 
# # Delete cluster
# ecs_svc$delete_cluster(cluster = "string")
