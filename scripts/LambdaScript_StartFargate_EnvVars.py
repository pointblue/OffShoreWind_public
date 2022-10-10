## Python script for Lambda function to start Fargate Task
##
## This version requires that a set of environment variables be set in the Lambda instance
## to determine the set-up of the Fargate task. Uses the boto3 package and the run_task
## function to start the Fargate task.

import boto3
import os

def lambda_handler(event,context):
    client = boto3.client('ecs')
    response = client.run_task(
        cluster=os.getenv('CLUSTER'),
        launchType=os.getenv('LAUNCH_TYPE', 'FARGATE'),
        taskDefinition=os.getenv('TASK_DEFINITION'),
        count=int(os.getenv('COUNT', 1)),
        platformVersion='LATEST',
        networkConfiguration={
            'awsvpcConfiguration': {
                'subnets': os.getenv('SUBNETS').split(','),
                'assignPublicIp': os.getenv('ASSIGN_PUBLIC_IP', 'ENABLED'),
                'securityGroups': os.getenv('SECURITY_GROUPS').split(','),
            },
        }
    )
    return str(response)
  
# Required env vars:
# $CLUSTER: name of the ECS cluster
# $TASK_DEFINITION: name and revision of the task definition (i.e. `mytask:1`)
# $SUBNETS: comma-separated list of subnets to place the new task
# $SECURITY_GROUPS: comma-separated list of security groups to be used for the new task
