# minion_project_sparse.R
# 
# Estimate either CJR or RSH-compete on either all votes or
# observed votes for each of a number of simulated legislatures.

# The project-specific source file does three things.
# First, it checks to see if needed packages are installed.
# If not, it tries to install them.  If it cannot load the required 
# packages, it reports back failure to this script.
# Second, if it can load the required packages, it completes the
# work unit assigned.  This likely involves downloading a
# project-specific task list with the parameters for the work
# unit, downloading data, etc.
# Third, it reports back to this script, either with success and an
# R object specifying data to be saved or with failure.

# Uses 'project.workunit'. Sets 'minion.success' (TRUE/FALSE), 'minion.output' (list), minion.output.file.name (character)

packages.loaded <- UsePackage("runjags") && UsePackage("abind") && UsePackage("pscl")
source("http://sheer.ucdavis.edu/svn/software/public/BugsFunctions/BugsFunctions.R")
source("http://sheer.ucdavis.edu/svn/software/public/RollCallFunctions/RollCallFunctions.R")

if(packages.loaded) {
  # Load workunit index:  'workunits'
  con <- url("http://sheer.ucdavis.edu/projects/abstentions/batch/minion_abs_sparse_inits_workunits.RData")
  load(con); close(con)
  
  # Load data: this.legislature
  con <- url(paste("http://sheer.ucdavis.edu/projects/abstentions/generated_legislature/sparse_abstentions/leg", 
    workunits$legislature[project.workunit], ".RData", sep=""))
  load(con); close(con)
  
  n.legislators <- this.legislature$gen.params$n.legislators
  n.item <- this.legislature$gen.params$n.bills


  if( "full" == workunits$mech[project.workunit] ) {
    jags.model <- "model { 
  for(i in 1:n.legislators) {
    for(j in 1:n.item) {
      rc[i,j] ~ dbern(p[i,j]);
      probit(p[i,j]) <- mu[i,j];
      mu[i,j] <- beta[j]*theta[i] - alpha[j];
    }
  }
  for(j in 1:n.item) { alpha[j] ~ dnorm(0, 0.25); }
  for(j in 1:n.item) { beta[j]  ~ dnorm(0, 0.25); }
  theta[1] <- -2;                  # spike leftmost legislator
  for(i in 2:(n.legislators-1)) { theta[i] ~ dnorm(0, 0.25); }
  theta[n.legislators] <- 2;      # spike rightmost legislator
}"
    if( "all" == workunits$votes[project.workunit] ) {
      rc <- this.legislature$rc.full
      minion.output.file.name <- paste("abs-sparse-inits-allvotes-CJR-leg", 
        workunits$legislature[project.workunit], ".RData", sep="")
    } else {
      rc <- this.legislature$"compete"$"0.05"$rc
      minion.output.file.name <- paste("abs-sparse-inits-obsvotes-CJR-leg", 
        workunits$legislature[project.workunit], ".RData", sep="")
    }
    jags.parameters = c("theta", "alpha", "beta")
    jags.data <- dump.format(list(rc=rc, n.legislators=n.legislators, n.item=n.item))
  } else {
    jags.model <- "model {
  for (i in 1:n.legislators) {
    for (j in 1:n.item) {
      y[i,j] ~ dcat(Q[i,j,1:3]);         # code Nay as 1, NA as 2, Aye as 3
      Q[i,j,1] <- pow(probVoteDisagree*(1-CJR[i,j]), p[i,j]) * pow(probVoteAgree*(1-CJR[i,j]), (1 - p[i,j]));
      Q[i,j,2] <- 1 - Q[i,j,1] - Q[i,j,3];
      Q[i,j,3] <- pow(probVoteAgree*CJR[i,j], p[i,j]) * pow(probVoteDisagree*CJR[i,j], (1 - p[i,j]));
      probit(CJR[i,j]) <- beta[j]*theta[i] - alpha[j];
      p[i,j] <- lead[party.membership[i],j];
    }
  }
  # priors
  logit(probVoteAgree) <- delta[1]+delta[2];
  logit(probVoteDisagree) <- delta[1];
  delta[1] ~ dnorm(0,0.1);
  delta[2] ~ dexp(0.1); #dexp(.01);
  for(j in 1:n.item) { alpha[j]  ~ dnorm(0, 0.25); }
  for(j in 1:n.item) { beta[j] ~ dnorm(0, 0.25); }
  theta[1] <- -2;                  # spike leftmost legislator
  for(i in 2:(n.legislators-1)) { theta[i] ~ dnorm(0, 0.25); }
  theta[n.legislators] <- 2;      # spike rightmost legislator
}"
    if( "all" == workunits$votes[project.workunit] ) {
      rc <- this.legislature$rc.full
      minion.output.file.name <- paste("abs-sparse-inits-allvotes-RSHcompete-leg", 
        workunits$legislature[project.workunit], ".RData", sep="")
    } else {
      rc <- this.legislature$"compete"$"0.05"$rc
      minion.output.file.name <- paste("abs-sparse-inits-obsvotes-RSHcompete-leg", 
        workunits$legislature[project.workunit], ".RData", sep="")
    }
    # Transform data for WinBugs input
    y <- ifelse(is.na(rc)==TRUE, 2, ifelse(rc==0, 1, 3))    # code NA as 2, Nay as 1, Aye as 3
    y.is.aye <- ifelse(y==3, 1, 0)                          # code Aye as 1, Nay/abstain as 0
    m.voted <- ifelse (is.na(rc)==TRUE, 0, 1)               # code Aye/Nay as 1, abstain as 0

    party.membership <- c(rep(1,this.legislature$gen.params$n.left), rep(2,this.legislature$gen.params$n.right))
    # Add matrix of known party positions
    lead.L  <- ifelse(this.legislature$true.params$left.y.star>0,1,0)      # code party Aye as 1, Nay as 0 for each item
    lead.R  <- ifelse(this.legislature$true.params$right.y.star>0,1,0)     # code party Aye as 1, Nay as 0 for each item
    lead <- rbind(lead.L, lead.R)
    L.party <- 1 + (this.legislature$true.params$left.y.star  > 0)*2 # code party Aye as 1, Nay as 0 for each item
    R.party <- 1 + (this.legislature$true.params$right.y.star > 0)*2 # code party Aye as 1, Nay as 0 for each item
    rc.party <- rbind(L.party, R.party)
    
    jags.parameters = c("theta", "alpha", "beta", "delta")
    jags.data <- dump.format(list(y=y, lead=lead, party.membership=party.membership, n.legislators=n.legislators, n.item=n.item))
  }
  
  raw.inits <- InitializeIdeals(rollcall(rc), anchors=c(1,31))
  jags.inits <- rep(dump.format(list(
    theta=c(NA, raw.inits$ideal.points[2:30], NA),
    alpha=raw.inits$bill.params["alpha",],
    beta=raw.inits$bill.params["beta1",] )), 2)
  
  jags.result <- run.jags(
    model=jags.model,
    monitor=jags.parameters, 
    n.chains=2,
    inits=jags.inits,
#    monitor.deviance=TRUE,
    data=jags.data,
#    thin=1, burnin=10, sample=100,
#    check.conv=FALSE, plots=FALSE)
    thin=10, burnin=40000, sample=40000,
    check.conv=TRUE, plots=FALSE)
  
  # collect results
  minion.output <- list(
    minion.workunit=minion.workunit, 
    url.project.batch=url.project.batch,
    project.workunit=project.workunit,
    legislature=workunits$legislature[project.workunit],
    mech=workunits$mech[project.workunit],
    ab.rate="0.05",
    jags.result=jags.result)
  minion.success <- TRUE
}
# else: report failure
