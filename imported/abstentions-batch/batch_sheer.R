# Estimating ideal points with (us) and without (CJR) modeling abstention
# Call via source("http://tinyurl.com/putsx7") which points to http://sheer.ucdavis.edu/projects/abstentions/batch/batch_sheer.R

source("http://sheer.ucdavis.edu/svn/software/public/deCruft/deCruft.R")

source("http://sheer.ucdavis.edu/svn/software/public/usePackage/usePackage.R")    # like 'library' except that it first installs the package if necessary
usePackage("runjags")
usePackage("RCurl")    # Enables HTML POSTings to get and save workunits
usePackage("abind")    # 'abind' is like 'cbind' and 'rbind' except for arrays
source("http://sheer.ucdavis.edu/svn/software/public/postObjects/postObjects.R")
source("http://sheer.ucdavis.edu/svn/software/public/getWorkunit/getWorkunit.R")
source("http://sheer.ucdavis.edu/svn/software/public/BugsFunctions/BugsFunctions.R")

base.url.abstentions.repository <- "http://sheer.ucdavis.edu/projects/abstentions/"

#host.name <- readline("What is the name of this computer?  ")
# ADD CODE to store this both at checking out and checking in

get.workunit.project <- "abstentions"
get.workunit.from <- paste(base.url.abstentions.repository, "batch/checkout.php", sep="")
get.save.workunit.pw <- "odkpxmnyzvdaqfdgvgicpomhkubxxdcq"

workunit.control.file <- paste(base.url.abstentions.repository, "batch/workunits.RData", sep="")
con <- url(workunit.control.file); load(con); close(con)
remote.generated.legis.folder <- paste(base.url.abstentions.repository, "generated_legislature/", sep="")

upload.results.to <- paste(base.url.abstentions.repository, "batch/save.php", sep="")

# Functions
get.legislature <- function(i) {
  con <- url(paste(remote.generated.legis.folder, "leg", i, ".RData", sep=""))
  load(con)
  close(con)
  return( this.legislature )
}

base.RNGs <- c("base::Wichmann-Hill", "base::Marsaglia-Multicarry", 
  "base::Super-Duper", "base::Mersenne-Twister")

source(paste(base.url.abstentions.repository, "batch/all_models.jags", sep=""))
batch.status.good <- TRUE

##################################
###  MAIN LOOP OVER WORKUNITS  ###
##################################
while(batch.status.good) {

  # Choose a run.
  i.workunit <- as.numeric(getWorkunit(project=get.workunit.project, get.from.url=get.workunit.from, pw=get.save.workunit.pw))
  if(0 == i.workunit) {
    cat("No more workunits available.\n")
    break
  }
  cat("Starting workunit", i.workunit, "\n")
  
  i.legis      <- workunits$legislature[i.workunit]
  this.ab.rate <- workunits$ab.rate[i.workunit]
  this.ab.mech <- workunits$mech[i.workunit]
  
  # Prepare data
  leg <- get.legislature(i.legis)
  
  if( this.ab.mech == "cjrComplete") {
    rc <- leg$rc.full
  } else {
    rc <- leg[[this.ab.mech]][[as.character(this.ab.rate)]]$rc
  }
  
  n.legislators <- leg$gen.params$n.legislators
  n.item <- leg$gen.params$n.bills

  party.membership <- c(rep(1,leg$gen.params$n.left), rep(2,leg$gen.params$n.right))
  # Add matrix of known party positions
  #lead.L  <- sign(leg$true.params$left.y.star)      # code party Aye as 1, Nay as -1 for each item
  #lead.R  <- sign(leg$true.params$right.y.star)     # code party Aye as 1, Nay as -1 for each item
  lead.L  <- ifelse(leg$true.params$left.y.star>0,1,0)      # code party Aye as 1, Nay as 0 for each item
  lead.R  <- ifelse(leg$true.params$right.y.star>0,1,0)     # code party Aye as 1, Nay as 0 for each item
  lead <- rbind(lead.L, lead.R)
  L.party <- 1 + (leg$true.params$left.y.star  > 0)*2 # code party Aye as 1, Nay as 0 for each item
  R.party <- 1 + (leg$true.params$right.y.star > 0)*2 # code party Aye as 1, Nay as 0 for each item
  rc.party <- rbind(L.party, R.party) 

  # Transform data for WinBugs input
  y <- ifelse(is.na(rc)==TRUE, 2, ifelse(rc==0, 1, 3))    # code NA as 2, Nay as 1, Aye as 3
  y.is.aye <- ifelse(y==3, 1, 0)                          # code Aye as 1, Nay/abstain as 0
  m.voted <- ifelse (is.na(rc)==TRUE, 0, 1)               # code Aye/Nay as 1, abstain as 0
  
  if(this.ab.mech == "indiff") {
    # Prep for CJR (Observed-data) run
    cjr.jags.parameters = c("theta", "alpha", "beta", "deviance")
    cjr.jags.inits.1 <- dump.format(list(theta = c(NA, rnorm(n.legislators-2), NA), alpha = rnorm(n.item), beta  = rnorm(n.item) ))
    cjr.jags.inits.2 <- dump.format(list(theta = c(NA, rnorm(n.legislators-2), NA), alpha = rnorm(n.item), beta  = rnorm(n.item) ))
    cjr.jags.data <- dump.format(list(rc=rc, n.legislators=n.legislators, n.item=n.item))
    
  } else if (this.ab.mech == "extreme") {
    # Prep for CJR (Observed-data) run
    cjr.jags.parameters = c("theta", "alpha", "beta", "deviance")
    cjr.jags.inits.1 <- dump.format(list(theta = c(rnorm(1), NA, rnorm(n.legislators-4), NA, rnorm(1)), alpha = rnorm(n.item), beta  = rnorm(n.item) ))
    cjr.jags.inits.2 <- dump.format(list(theta = c(rnorm(1), NA, rnorm(n.legislators-4), NA, rnorm(1)), alpha = rnorm(n.item), beta  = rnorm(n.item) ))
    cjr.jags.data <- dump.format (list(rc=rc, n.legislators=n.legislators, n.item=n.item))

    # Prep for our (complete-data) run
    abstain.jags.parameters = c("theta", "alpha", "beta", "lambda", "deviance")
    abstain.jags.inits <-  sapply(1:2, function(i) {
      dump.format (list(
        theta = c(rnorm(1), NA, rnorm(n.legislators-4), NA, rnorm(1)), 
        alpha = rnorm(n.item),
        beta = rnorm(n.item), 
        lambda = runif(2),
        .RNG.name=sample(base.RNGs, 1), .RNG.seed=i )) })
    abstain.jags.data <- dump.format (list(rc=rc, m.voted=m.voted, n.legislators=n.legislators, n.item=n.item))
    
  } else if (this.ab.mech == "compete" | this.ab.mech == "competeNoMAR" | this.ab.mech == "competeMaj") {
    # Prep for CJR (Observed-data) run
      cjr.jags.parameters = c("theta", "alpha", "beta", "deviance")
      cjr.jags.inits.1 <- dump.format(list(theta = rnorm(n.legislators), alpha = rnorm(n.item), beta  = rnorm(n.item) ))
      cjr.jags.inits.2 <- dump.format(list(theta = rnorm(n.legislators), alpha = rnorm(n.item), beta  = rnorm(n.item) ))
      cjr.jags.data <- dump.format(list(rc=rc, n.legislators=n.legislators, n.item=n.item))
      
    # Prep for our (complete-data) run
      abstain.jags.parameters = c("theta", "alpha", "beta", "probVoteAgree", "probVoteDisagree", "deviance")
      abstain.jags.inits <- sapply(1:2, function(i) {
        dump.format(list(
          theta = c(-2, rnorm(n.legislators-2, 0, 0.5), 2),
          alpha = rnorm(n.item),
          beta = rnorm(n.item),
          delta = c(rnorm(1), rexp(1)),
          .RNG.name=sample(base.RNGs, 1), .RNG.seed=i )) })
      abstain.jags.data <- dump.format(list(y=y, lead=lead, party.membership=party.membership, n.legislators=n.legislators, n.item=n.item))
    
  } else if (this.ab.mech == "alien") {
    # Prep for CJR (Observed-data) run
    cjr.jags.parameters = c("theta", "alpha", "beta", "deviance")
    cjr.jags.inits.1 <- dump.format(list(ystar = apply(y, c(1,2), function(this.y) runif(1) * (this.y-2)), theta = c(NA, rnorm(n.legislators-2), NA), alpha = rnorm(n.item), beta  = rnorm(n.item) ))
    cjr.jags.inits.2 <- dump.format(list(ystar = apply(y, c(1,2), function(this.y) runif(1) * (this.y-2)), theta = c(NA, rnorm(n.legislators-2), NA), alpha = rnorm(n.item), beta  = rnorm(n.item) ))
    cjr.jags.data <- dump.format(list(rc=rc, n.legislators=n.legislators, n.item=n.item, str.norm.draws=rnorm(25000)))
    
    # Prep for our (complete-data) run
    abstain.jags.parameters = c("theta", "aye", "nay", "tau", "sigma", "deviance")
    abstain.jags.inits <- sapply(1:2, function(i) {
      dump.format(list(
        theta = c(-2, rnorm(n.legislators-2, 0, 0.5), 2),
        alpha = rnorm(n.item),
        beta = rnorm(n.item),
        aye = rnorm(n.item),
        nay = rnorm(n.item),
        sigma = rexp(n.item, min=0, max=0.25),
        .RNG.name=sample(base.RNGs, 1), .RNG.seed=i )) })
    abstain.jags.data <- dump.format (list(y=y, n.legislators=n.legislators, n.item=n.item, draws.eta=rnorm(25000), draws.nu=rnorm(25000)))
  
  } else if (this.ab.mech == "cjrComplete") {
    
    # Prep for CJR (all data) run
    cjr.jags.parameters = c("theta", "alpha", "beta", "deviance")
    cjr.jags.inits.1 <- dump.format(list(theta = c(NA, rnorm(n.legislators-2), NA), alpha = rnorm(n.item), beta  = rnorm(n.item) ))
    cjr.jags.inits.2 <- dump.format(list(theta = c(NA, rnorm(n.legislators-2), NA), alpha = rnorm(n.item), beta  = rnorm(n.item) ))
    cjr.jags.data <- dump.format(list(rc=rc, n.legislators=n.legislators, n.item=n.item))
    
  } else {
    stop("No other mechanisms configured.")
  }

  jags.result.cjr <- run.jags (
    model=models$cjr[[this.ab.mech]],
    monitor=cjr.jags.parameters, n.chains=2,
    monitor.pd=TRUE,
    data=cjr.jags.data,
    inits=c(cjr.jags.inits.1, cjr.jags.inits.2),
    thin=10, burnin=5000, sample=15000,
#    thin=1, burnin=5, sample=15,
    check.conv=TRUE, plots=FALSE)
    
  if(this.ab.mech == "indiff") {

    # Prep for our (complete-data) run
    abstain.jags.parameters <- c("theta", "alpha", "beta", "gamma", "sigma", "deviance")

    cjr.draws <- ExtractCodaDraws(jags.result.cjr$mcmc)
    abstain.jags.inits <- sapply(1:2, function(i) {
      dump.format(list(
        theta = c(-2, colMeans(cjr.draws[,grep("theta", names(cjr.draws))[2:30]]), 2),
        alpha = colMeans(cjr.draws[,grep("alpha", names(cjr.draws))]),
        beta = colMeans(cjr.draws[,grep("beta", names(cjr.draws))]),
        gamma = runif(n.legislators, min=.5, max=2),
        sigma = c(1, runif(n.item-1, min=.5, max=2)),
        .RNG.name=base.RNGs[i], .RNG.seed=1 )) })
    abstain.jags.data <- dump.format(list(y=y, n.legislators=n.legislators, n.item=n.item))
  }
  
  if( this.ab.mech != "cjrComplete") {
    jags.result.abstain <- run.jags(
      model=models$abstain[[this.ab.mech]],
      monitor=abstain.jags.parameters,
      n.chains=2,
      monitor.pd=TRUE,
      data=abstain.jags.data,
      inits=abstain.jags.inits,  # This line can cause problems
#      thin=1, burnin=5, sample=15, check.conv=FALSE, plots=FALSE)
      thin=10, burnin=5000, sample=15000,
#      thin=1, burnin=5, sample=15,
      check.conv=TRUE, plots=FALSE)
  }
  
  jags.cjr <- list(mcmc=jags.result.cjr$mcmc, psrf=jags.result.cjr$psrf$psrf, pd=jags.result.cjr$pd)
  if( this.ab.mech != "cjrComplete") jags.abstain <- list(mcmc=jags.result.abstain$mcmc, psrf=jags.result.abstain$psrf$psrf, pd=jags.result.abstain$pd)
  else jags.abstain <- NA
  
  job.comments <- c("all_models.jags date: 3/26/10",
    "Live runs")
  
  batch.status.good <- postObjects(
    filename=paste(this.ab.mech, "-leg", i.legis, "-rate", this.ab.rate, ".RData", sep=""), 
    save.to.URL=upload.results.to, 
    post.params=list(pw=get.save.workunit.pw, project=get.workunit.project, workunit=as.character(i.workunit)), 
    jags.cjr, jags.abstain, job.comments)
}
