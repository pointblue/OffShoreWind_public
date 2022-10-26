## Python script for Lambda function to start Fargate Task
##
## This version hard-codes the set-up of the Fargate task. The Uses the boto3 package and the run_task
## function to start the Fargate task.


import boto3
import os

def lambda_handler(event,context):
    client = boto3.client('ecs')
    response = client.run_task(
        cluster='r-fargito-skelito-cluster',
        launchType='FARGATE',
        taskDefinition='r-fargito-skelito-td',
        count= 1,
        platformVersion='LATEST',
        networkConfiguration={
            'awsvpcConfiguration': {
                'subnets': ['subnet-0386fb9f7e0eaabc5'],
                'assignPublicIp': 'ENABLED'
            },
        },

    )
    return str(response)
  
####
# overrides={
#         'containerOverrides': [
#             {
#                 'name': 'string',
#                 'command': [
#                     'string',
#                 ],
#                 'environment': [
#                     {
#                         'name': 'string',
#                         'value': 'string'
#                     },
#                 ],
#                 'environmentFiles': [
#                     {
#                         'value': 'string',
#                         'type': 's3'
#                     },
#                 ],
#             },
#         ],
#         'cpu': 'string',
#         'executionRoleArn': 'string',
#         'memory': 'string',
#         'taskRoleArn': 'string',
#         'ephemeralStorage': {
#             'sizeInGiB': 123
#         }
#     }
