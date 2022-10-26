# Code to set up and initiate a Lambda Function

library("paws")

#-------------------------------------------------------------------------------
# Create a Lambda function package to upload.  ##### THIS SHOULD BE THE FILE CONTAINING THE LAMBDA SCRIPT, NOT INLINE CODE

code <- "import boto3
import os

def lambda_handler(event,context):
    client = boto3.client('ecs')
    response = client.run_task(
        cluster='test-greet-cluster',
        launchType='FARGATE',
        taskDefinition='test-greet-cluster',
        count= 1,
        platformVersion='LATEST',
        networkConfiguration={
            'awsvpcConfiguration': {
                'subnets': ['subnet-0e86140db2cd142ae'],
                'assignPublicIp': ['ENABLED']
            },
        },
        overrides={
        'containerOverrides': [
            {
                'name': 'test-greet',
                'environment': [
                    {
                        'name': 'HI_TO',
                        'value': 'Cotton'
                    },],
             },],
        },
    )
    return str(response)"

path <- tempdir()
py_file <- file.path(path, "lambda.py")
writeLines(code, py_file)

zip_file <- file.path(path, "lambda.zip")
utils::zip(zip_file, py_file, flags = "-j") # flag -j stores just the file names not the path or directory names
zip_contents <- readBin(zip_file, "raw", n = 1e5)

#-------------------------------------------------------------------------------
# Set up an IAM role for the Lambda function.

role_name <- "Lambda_test-greet_role"  # GIVE THIS A BETTER NAME
policy_arn <- "arn:aws:iam::aws:policy/service-role/AWSLambdaBasicExecutionRole"  # SHOULD WE USE THIS PROVIDED ROLE OR IS THERE A DIFFERENT ONE WE NEED?

trust_policy <- list(
  Version = "2012-10-17",
  Statement = list(
    list(
      Effect = "Allow",
      Principal = list(
        Service = "lambda.amazonaws.com"
      ),
      Action = "sts:AssumeRole"  # IS THIS CORRECT?
    )
  )
)

iam <- paws::iam(config = list(
  region = "us-east-2" # need to specify region as different from profile default
  )
)

role <- iam$create_role(
  RoleName = role_name,
  AssumeRolePolicyDocument = jsonlite::toJSON(trust_policy, auto_unbox = TRUE)
)

iam$attach_role_policy(
  RoleName = role_name,
  PolicyArn = policy_arn
)

#-------------------------------------------------------------------------------

lambda <- paws::lambda(config = list(
  region = "us-east-2" # need to specify region as different from profile default
  )
)

# Create the Lambda function.
lambda$create_function(
  Code = list(ZipFile = zip_contents),
  FunctionName = "Test-greet-lambdafunc",
  Handler = "lambda.lambda_handler", # this refers to the file where the handler is stored (lambda.py) and the function name (lambda_handler) formatted as `file.function`
  Role = role$Role$Arn,
  Runtime = "python3.9" # latest python Lambda runtime
)

##################################### THIS IS ONLY IF WE WANT TO RUN THE LAMBDA PROGRAMMATICALLY INSTEAD OF VIA S3 OR OTHER TRIGGER

# Run the function.
resp <- lambda$invoke("Test-greet-lambdafunc")

# Print the function's output.
rawToChar(resp$Payload)

# List available functions.
lambda$list_functions()

# Clean up.
lambda$delete_function("Test-greet-lambdafunc")
iam$detach_role_policy(role_name, policy_arn)
iam$delete_role(role_name)
