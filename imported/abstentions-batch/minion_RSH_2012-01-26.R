# minion_RSH_2012-01-26.R
# 
# Estimate NOMINATE, CJR, or RSH-compete on either all votes or
# observed votes for each of a number of simulated legislatures.

# The project-specific source file does three things.
# First, it checks to see if needed packages are installed.
# If not, it tries to install them.  If it cannot load the required 
# packages, it reports back failure to do_workunit.R.
# Second, if it can load the required packages, it completes the
# work unit assigned.  This likely involves downloading a
# project-specific task list with the parameters for the work
# unit, downloading data, etc.
# Third, it reports back to do_workunit.R, either with success and an
# R object specifying data to be saved or with failure.

# Uses 'project.workunit'. Sets 'minion.success' (TRUE/FALSE), 'minion.output' (list), minion.output.file.name (character)

setwd("~")

packages.loaded <- UsePackage("MCMCpack") && UsePackage("xtable") && UsePackage("wnominate") && UsePackage("runjags") && UsePackage("abind") && UsePackage("pscl")
source("http://sheer.ucdavis.edu/svn/software/public/BugsFunctions/BugsFunctions.R")

# temp fix for old runjags
success <- TRUE

if(packages.loaded) {
  # Load workunit index:  'workunits'
  con <- url("http://sheer.ucdavis.edu/projects/abstentions/batch/minion_RSH_2012-01-26_workunits.RData")
  load(con); close(con)
  
  # Load data: this.legislature
  con <- url(paste("http://sheer.ucdavis.edu/projects/abstentions/generated_legislature/RSH_2012-01-26/leg", 
    workunits$legislature[project.workunit], ".RData", sep=""))
  load(con); close(con)
  
  n.legislators <- this.legislature$gen.params$n.legislators
  n.item <- this.legislature$gen.params$n.bills

  if( "full" == workunits$mech[project.workunit] ) {
    # Run CJR on full data using MCMCpack
    rc <- this.legislature$rc.full
    constraints <- list(-2, 2)
    names(constraints) <- c("V1", paste("V", n.legislators, sep=""))
    result <- MCMCirt1d(rc,
      theta.constraints=constraints,
      store.item=TRUE,
      verbose=10000,
      thin=50,
      burnin=20000,
      mcmc=50000)
    result.type <- "MCMCirt1d"
    minion.output.file.name <- paste("RSH_2012-01-26/RSH_2012-01-26_CJR_on_all_votes_leg", 
      workunits$legislature[project.workunit], ".RData", sep="")
  } else {
    rc <- this.legislature[[workunits$mech[project.workunit]]][[as.character(workunits$ab.rate[project.workunit])]]$rc
    
    if( "nominate" == workunits$model[project.workunit] ) {
      ##################
      ###  NOMINATE  ###
      ##################
      result <- wnominate(rcObject=rollcall(rc),
        dims=1,
        polarity=n.legislators,
        verbose=TRUE,
        trials=50,
        minvotes=ifelse(30==n.item, 12, 20))
      result.type <- "wnominate"
      minion.output.file.name <- paste("RSH_2012-01-26/RSH_2012-01-26_NOM_on_", 
        workunits$mech[project.workunit], "_at_", 
        workunits$ab.rate[project.workunit], "_leg", 
        workunits$legislature[project.workunit], ".RData", sep="")
    } else if( "cjr" == workunits$model[project.workunit] ) {
      #############
      ###  CJR  ###
      #############
      constraints <- list(-2, 2)
      names(constraints) <- c("V1", paste("V", n.legislators, sep=""))
      result <- MCMCirt1d(rc,
        theta.constraints=constraints,
        store.item=TRUE,
        verbose=10000,
        thin=50,
        burnin=20000,
        mcmc=50000)
      result.type <- "MCMCirt1d"
      minion.output.file.name <- paste("RSH_2012-01-26/RSH_2012-01-26_CJR_on_", 
        workunits$mech[project.workunit], "_at_", 
        workunits$ab.rate[project.workunit], "_leg", 
        workunits$legislature[project.workunit], ".RData", sep="")
    } else {  # model is RSH
      #############
      ###  RSH  ###
      #############
      y <- ifelse(is.na(rc)==TRUE, 2, ifelse(rc==0, 1, 3))    # code NA as 2, Nay as 1, Aye as 3
      lead.L  <- ifelse(this.legislature$true.params$left.y.star>0,1,0)      # code party Aye as 1, Nay as 0 for each item
      lead.R  <- ifelse(this.legislature$true.params$right.y.star>0,1,0)     # code party Aye as 1, Nay as 0 for each item
      lead <- rbind(lead.L, lead.R)
      party.membership <- c(rep(1,this.legislature$gen.params$n.left), rep(2,this.legislature$gen.params$n.right))

      if( "compete" == workunits$mech[project.workunit] ) {
        ###  Fit RSH:compete  ###
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
  probVoteAgree ~ dbeta(1,1);
  probVoteDisagree ~ dbeta(1,1);
  for(j in 1:n.item) { alpha[j]  ~ dnorm(0, 0.25); }
  for(j in 1:n.item) { beta[j] ~ dnorm(0, 0.25); }
  theta[1] <- -2;                 # spike leftmost legislator
  for(i in 2:(n.legislators-1)) { theta[i] ~ dnorm(0, 1); }
  theta[n.legislators] <- 2;      # spike rightmost legislator
}"
        jags.parameters <- c("theta", "alpha", "beta", "probVoteAgree", "probVoteDisagree", "deviance")
        jags.data <- dump.format(list(y=y, n.legislators=n.legislators, n.item=n.item, lead=lead, party.membership=party.membership))
      } else {
        ###  Fit RSH:indiff  ###
        jags.model <- "model {
  for (i in 1:n.legislators) {
    for (j in 1:n.item) {
      y[i,j] ~ dcat(p[i,j,1:3]);
      p[i,j,1] <- 1 - Q[i,j,1];         # Pr(Nay observed)
      p[i,j,2] <- Q[i,j,1] - Q[i,j,2];  # Pr(abstain)
      p[i,j,3] <- Q[i,j,2];             # Pr(Aye observed)
      probit(Q[i,j,1]) <- beta[j]*theta[i] - alpha[j] + gamma[i]/sigma[j];
      probit(Q[i,j,2]) <- beta[j]*theta[i] - alpha[j] - gamma[i]/sigma[j];
    }
  }
  # PRIORS
  for(j in 1:n.item) { alpha[j] ~ dnorm(0, 0.25); }
  for(j in 1:n.item) { beta[j]  ~ dnorm(0, 0.25); }
  sigma[1] <- 1;
  for(j in 2:n.item) { sigma[j]  ~ dexp(0.1); }
  theta[1] <- -2;
  for(i in 2:(n.legislators-1)) { theta[i] ~ dnorm(0, 1); }
  theta[n.legislators] <- 2;
  for (i in 1:n.legislators) { gamma[i] ~ dexp(0.1); }
}"
        jags.parameters <- c("theta", "alpha", "beta", "gamma", "sigma", "deviance")
        jags.data <- dump.format(list(y=y, n.legislators=n.legislators, n.item=n.item))
      }
      result <- run.jags(
        model=jags.model,
        monitor=jags.parameters, 
        n.chains=2,
        monitor.deviance=TRUE,
        data=jags.data,
#        thin=1, burnin=10, sample=100,
#        check.conv=FALSE, plots=FALSE)
        thin=10, burnin=40000, sample=40000,
        check.conv=TRUE, plots=FALSE)
      
      result.type <- "RSH"
      minion.output.file.name <- paste("RSH_2012-01-26/RSH_2012-01-26_RSH_on_", 
        workunits$mech[project.workunit], "_at_", 
        workunits$ab.rate[project.workunit], "_leg", 
        workunits$legislature[project.workunit], ".RData", sep="")
    }
  }
  
  # collect results
  minion.output <- list(
    minion.workunit=minion.workunit, 
    url.project.batch=url.project.batch,
    project.workunit=project.workunit,
    legislature=workunits$legislature[project.workunit],
    mech=workunits$mech[project.workunit],
    model=workunits$model[project.workunit],
    ab.rate=workunits$ab.rate[project.workunit],
    result.type=result.type,
    result=result)
  minion.success <- TRUE
}
# else: report failure
