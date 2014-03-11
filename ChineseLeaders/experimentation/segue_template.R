library(segue)


setCredentials(awsAccessKeyText=readline(prompt="Enter the AWS Access Key: "),
               awsSecretKeyText=readline(prompt="Enter the AWS Secret Key: "))

my.cluster <- createCluster(numInstances=2,  # number of nodes (EC2 instances)
                            cranPackages,    # vector of string names of CRAN packages to load on each cluster node
                            customPackages,  # vector of string file names of custom packages to load on each cluster node
                            filesOnNodes,    # vector of string names of full path of files to be loaded on each node. Files will be loaded into the local path (i.e. ./file) on each node.
                            rObjectsOnNodes, # a named list of R objects which will be passed to the R session on the worker nodes. Be sure the list has names. The list will be attached on the remote nodes using attach(rObjectsOnNodes). If you list does not have names, this will fail.
                            enableDebugging=FALSE,  # T/F whether EMR debugging should be enabled
                            instancesPerNode,       # Number of R instances per node. Default of NULL uses AWS defaults.
                            masterInstanceType="m1.large",  # EC2 instance type for the master node
                            slaveInstanceType="m1.large",   # EC2 instance type for the slave nodes
                            location="us-east-1c",  # EC2 location name for the cluster
                            ec2KeyName,             # EC2 Key used for logging into the main node. Use the user name 'hadoop'
                            copy.image=FALSE,       # T/F whether to copy the entire local environment to the nodes. If this feels fast and loose... you're right! It's nuts. Use it with caution. Very handy when you really need it.
                            otherBootstrapActions,  # a list-of-lists of other bootstrap actions to run; chlid list members
                            sourcePackagesToInstall,        # vector of full paths to source packages to be installed on each node
                            masterBidPrice,         # Bid price for master server
                            slaveBidPrice)          # Bid price for slave (task) server

cluster.job.flow.id <- startCluster(my.cluster)

# prep one or more jobs
emrlapply(clusterObject=my.cluster,
          X,
          FUN,
          taskTimeout=10,
          ...)

# send the jobs off to Amazon and run them
submitJob(clusterObject=my.cluster,
          stopClusterOnComplete=FALSE, 
          taskTimeout=10)

checkStatus(jobFlowId=cluster.job.flow.id)

stopCluster(my.cluster)