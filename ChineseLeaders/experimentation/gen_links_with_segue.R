library(segue)

# R objects to be passed to EMR
china <- readRDS("china_data_cleaned.rds")
n <- nrow(china$officials)

# DEBUG
n <- 3

setCredentials(awsAccessKeyText=readline(prompt="Enter the AWS Access Key: "),
               awsSecretKeyText=readline(prompt="Enter the AWS Secret Key: "))

my.cluster <- createCluster(numInstances=1,  # number of nodes (EC2 instances)
                            rObjectsOnNodes=c("china", "n"))

# cache one or more jobs
IsCloseInAge <- function(workunit.id) {
  workunits <- expand.grid(from=1:n, 
                           to=1:n)
  from.id <- workunits$from[workunit.id]
  to.id <- workunits$to[workunit.id]
  
  age.difference <- abs(china$officials$birthyear[from.id] -
    china$officials$birthyear[to.id])
  if( is.na(age.difference) || age.difference > 5 ) {
    return(0)
  } else {
    return(1)
  }
}
# Test: IsCloseInAge(2)

link.list <- emrlapply(clusterObject=my.cluster,
                       X=as.list(1:(n*n)),
                       FUN=IsCloseInAge)

checkStatus(jobFlowId=my.cluster$jobFlowId)

stopCluster(my.cluster)