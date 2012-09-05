# Estimating ideal points with (us) and without (CJR) modeling abstention

source("http://svn.wustl.edu/srhapton/software/software/public/deCruft/deCruft.R")

source("http://svn.wustl.edu/srhapton/software/software/public/usePackage/usePackage.R")
usePackage("runjags")
usePackage("RCurl")    # Enables HTML POSTings to get and save workunits
usePackage(abind)      # 'abind' is like 'cbind' and 'rbind' except for arrays
source("http://svn.wustl.edu/srhapton/software/software/public/postObjects/postObjects.R")
source("http://svn.wustl.edu/srhapton/software/software/public/getWorkunit/getWorkunit.R")

base.url.abstentions.repository <- "http://tadano.wustl.edu/abstentions/"

get.workunit.project <- "abstentions"
get.workunit.from <- paste(base.url.abstentions.repository, "batch/checkout.php", sep="")
get.save.workunit.pw <- "sqpykwkzfkbqdccrbautwnzuohswlu"

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
  i.legis      <- workunits$legislature[i.workunit]
  this.ab.rate <- workunits$ab.rate[i.workunit]
  this.ab.mech <- workunits$mech[i.workunit]
  
  # Prepare data
  leg <- get.legislature(i.legis)
  rc <- leg[[this.ab.mech]][[as.character(this.ab.rate)]]$rc
  n.legislators <- leg$gen.params$n.legislators
  n.item <- leg$gen.params$n.bills

  party.membership <- c(rep(1,leg$gen.params$n.left), rep(2,leg$gen.params$n.right))
  # Add matrix of known party positions
  # NEED TO CHANGE THIS: CODE PARTY POSITIONS AS 1=AYE, 0=NAY (i.e., drop sign function)
  # lead.L  <- sign(leg$true.params$left.y.star)      # code party Aye as 1, Nay as -1 for each item
  # lead.R  <- sign(leg$true.params$right.y.star)     # code party Aye as 1, Nay as -1 for each item
  lead.L  <- ifelse(leg$true.params$left.y.star>0,1,0)      # code party Aye as 1, Nay as -1 for each item
  lead.R  <- ifelse(leg$true.params$right.y.star>0,1,0)     # code party Aye as 1, Nay as -1 for each item
  lead <- rbind(lead.L, lead.R)

  # Transform data for WinBugs input
  y <- ifelse(is.na(rc)==TRUE, 2, ifelse(rc==0, 1, 3))    # code NA as 2, Nay as 1, Aye as 3
  y.is.aye <- ifelse(y==3, 1, 0)                          # code Aye as 1, Nay/abstain as 0
  m.voted <- ifelse (is.na(rc)==TRUE, 0, 1)               # code Aye/Nay as 1, abstain as 0
  
# BOUNDARIES NO LONGER NEEDED
  # Set truncation boundaries for y.star and m.star
#  truncation.range <- 75
#  lower.y <- ifelse(is.na(rc), -truncation.range, ifelse(rc==0, -truncation.range, 0))
#  upper.y <- ifelse(is.na(rc), truncation.range, ifelse(rc==0, 0, truncation.range))
#  lower.m <- ifelse(m.voted==0, -truncation.range, 0)
#  upper.m <- ifelse(m.voted==0, 0, truncation.range)
#  const.y <- abind(lower.y, upper.y, along=3)
#  const.m <- abind(lower.m, upper.m, along=3)
# , lower.y=lower.y, upper.y=upper.y, lower.m=lower.m, upper.m=upper.m
  
  if(this.ab.mech == "indiff") {
    # Prep for CJR (Observed-data) run
    cjr.jags.parameters = c("theta", "alpha", "beta")
    cjr.jags.inits.1 <- dump.format(list(theta = rnorm(n.legislators), alpha = rnorm(n.item), beta  = rnorm(n.item) ))
    cjr.jags.inits.2 <- dump.format(list(theta = rnorm(n.legislators), alpha = rnorm(n.item), beta  = rnorm(n.item) ))
    cjr.jags.data <- dump.format(list(rc=rc, n.legislators=n.legislators, n.item=n.item))
#    cjr.jags.inits.1 <- dump.format(list(ystar = apply(y, c(1,2), function(this.y) runif(1) * (this.y-2)), theta = rnorm(n.legislators), alpha = rnorm(n.item), beta  = rnorm(n.item) ))
#    cjr.jags.inits.2 <- dump.format(list(ystar = apply(y, c(1,2), function(this.y) runif(1) * (this.y-2)), theta = rnorm(n.legislators), alpha = rnorm(n.item), beta  = rnorm(n.item) ))
#    cjr.jags.data <- dump.format(list(rc=rc, n.legislators=n.legislators, n.item=n.item, lower.y=lower.y, upper.y=upper.y, lower.m=lower.m, upper.m=upper.m))
    
    # Prep for our (complete-data) run
    abstain.jags.parameters = c("theta", "alpha", "beta", "cut")
    abstain.jags.inits.1 <- dump.format(list(theta = rnorm(n.legislators), alpha = rnorm(n.item), beta = rnorm(n.item), 
      cut = matrix(data=cbind (rep(NA, n.legislators), runif (n.legislators,0,0.2) ), byrow=F, ncol=2) ))
    abstain.jags.inits.2 <- dump.format(list(theta = rnorm(n.legislators), alpha = rnorm(n.item), beta = rnorm(n.item),
      cut = matrix(data = cbind (rep(NA, n.legislators), runif (n.legislators,0,0.2) ), byrow=F, ncol=2) ))
    abstain.jags.data <- dump.format(list(y.is.aye=y.is.aye, m.voted=m.voted, y=y, n.legislators=n.legislators, n.item=n.item))
#    abstain.jags.inits.1 <- dump.format(list(theta = rnorm(n.legislators), alpha = rnorm(n.item), beta = rnorm(n.item), 
#      cut = matrix(data=cbind (rep(NA, n.legislators), runif (n.legislators,0,0.2) ), byrow=F, ncol=2) ))
#    abstain.jags.inits.2 <- dump.format(list(theta = rnorm(n.legislators), alpha = rnorm(n.item), beta = rnorm(n.item),
#      cut = matrix(data = cbind (rep(NA, n.legislators), runif (n.legislators,0,0.2) ), byrow=F, ncol=2) ))
#    abstain.jags.data <- dump.format(list(y.is.aye=y.is.aye, m.voted=m.voted, y=y, n.legislators=n.legislators, n.item=n.item, 
#      lower.y=lower.y, upper.y=upper.y, lower.m=lower.m, upper.m=upper.m))
    
  } else if (this.ab.mech == "extreme") {
    # Prep for CJR (Observed-data) run
    cjr.jags.parameters = c("theta", "alpha", "beta")
    cjr.jags.inits.1 <- dump.format(list(theta = rnorm(n.legislators), alpha = rnorm(n.item), beta  = rnorm(n.item) ))
    cjr.jags.inits.2 <- dump.format(list(theta = rnorm(n.legislators), alpha = rnorm(n.item), beta  = rnorm(n.item) ))
    cjr.jags.data <- dump.format (list(rc=rc, n.legislators=n.legislators, n.item=n.item))
#    cjr.jags.inits.1 <- dump.format(list(ystar = apply(y, c(1,2), function(this.y) runif(1) * (this.y-2)), theta = rnorm(n.legislators), alpha = rnorm(n.item), beta  = rnorm(n.item) ))
#    cjr.jags.inits.2 <- dump.format(list(ystar = apply(y, c(1,2), function(this.y) runif(1) * (this.y-2)), theta = rnorm(n.legislators), alpha = rnorm(n.item), beta  = rnorm(n.item) ))
#    cjr.jags.data <- dump.format (list(rc=rc, n.legislators=n.legislators, n.item=n.item, const.y=const.y))
#    cjr.jags.data <- dump.format (list(rc=rc, n.legislators=n.legislators, n.item=n.item, lower.y=lower.y, upper.y=upper.y))

    # Prep for our (complete-data) run
#    ystar.1 <- apply(y, c(1,2), function(this.y) runif(1) * (this.y-2))
#    ystar.2 <- apply(y, c(1,2), function(this.y) runif(1) * (this.y-2))
#    mstar.1 <- apply(m.voted, c(1,2), function(did.vote) ifelse(did.vote==1, runif(1), -1*runif(1)))
#    mstar.2 <- apply(m.voted, c(1,2), function(did.vote) ifelse(did.vote==1, runif(1), -1*runif(1)))
#    L <- diag(3)
    abstain.jags.parameters = c("theta", "alpha", "beta", "lambda")
    abstain.jags.inits.1 <- dump.format (list(theta = rnorm(n.legislators), alpha = rnorm(n.item),
      beta = rnorm(n.item), lambda = runif(3) ))
    abstain.jags.inits.2 <- dump.format (list(theta = rnorm(n.legislators), alpha = rnorm(n.item),
      beta = rnorm(n.item), lambda = runif(3) ))
    abstain.jags.data <- dump.format (list(rc=rc, m.voted=m.voted, n.legislators=n.legislators, n.item=n.item))
#    abstain.jags.inits.1 <- dump.format (list(zstar = abind(ystar.1, mstar.1, along=3), theta = rnorm(n.legislators), alpha = rnorm(n.item),
#      beta = rnorm(n.item), lambda = rnorm(3) ))
#    abstain.jags.inits.2 <- dump.format (list(zstar = abind(ystar.2, mstar.2, along=3), theta = rnorm(n.legislators), alpha = rnorm(n.item),
#      beta = rnorm(n.item), lambda = rnorm(3) ))
#    abstain.jags.data <- dump.format (list(rc=rc, m.voted=m.voted, n.legislators=n.legislators, n.item=n.item, L=L, 
#    abstain.jags.data <- dump.format (list(rc=rc, m.voted=m.voted, n.legislators=n.legislators, n.item=n.item, lower.y=lower.y, upper.y=upper.y, lower.m=lower.m, upper.m=upper.m))
    
  } else if (this.ab.mech == "compete") {
    # Prep for CJR (Observed-data) run
    cjr.jags.parameters = c("theta", "alpha", "beta")
    cjr.jags.inits.1 <- dump.format(list(ystar = apply(y, c(1,2), function(this.y) runif(1) * (this.y-2)), theta = rnorm(n.legislators), alpha = rnorm(n.item), beta  = rnorm(n.item) ))
    cjr.jags.inits.2 <- dump.format(list(ystar = apply(y, c(1,2), function(this.y) runif(1) * (this.y-2)), theta = rnorm(n.legislators), alpha = rnorm(n.item), beta  = rnorm(n.item) ))
    cjr.jags.data <- dump.format(list(rc=rc, n.legislators=n.legislators, n.item=n.item, 
      lower.y=lower.y, upper.y=upper.y, lower.m=lower.m, upper.m=upper.m))
#    cjr.jags.parameters = c("theta", "alpha", "beta")
#    cjr.jags.inits.1 <- dump.format(list(ystar = apply(y, c(1,2), function(this.y) runif(1) * (this.y-2)), theta = rnorm(n.legislators), alpha = rnorm(n.item), beta  = rnorm(n.item) ))
#    cjr.jags.inits.2 <- dump.format(list(ystar = apply(y, c(1,2), function(this.y) runif(1) * (this.y-2)), theta = rnorm(n.legislators), alpha = rnorm(n.item), beta  = rnorm(n.item) ))
#    cjr.jags.data <- dump.format(list(rc=rc, n.legislators=n.legislators, n.item=n.item, 
#      lower.y=lower.y, upper.y=upper.y, lower.m=lower.m, upper.m=upper.m))
    
    # Prep for our (complete-data) run
    abstain.jags.parameters = c("theta", "alpha", "beta", "delta", "gamma")
    abstain.jags.inits.1 <- dump.format(list(theta = rnorm(n.legislators), alpha = rnorm(n.item),
      beta = rnorm(n.item), delta = runif(n.item), gamma = rnorm(n.item) ))
    abstain.jags.inits.2 <- dump.format(list(theta = rnorm(n.legislators), alpha = rnorm(n.item),
      beta = rnorm(n.item), delta = runif(n.item), gamma = rnorm(n.item) ))
    abstain.jags.data <- dump.format(list(rc=rc, m.voted=m.voted, lead=lead, party.membership=party.membership, n.legislators=n.legislators, n.item=n.item))
#    ystar.1 <- apply(y, c(1,2), function(this.y) runif(1) * (this.y-2))
#    ystar.2 <- apply(y, c(1,2), function(this.y) runif(1) * (this.y-2))
#    mstar.1 <- apply(m.voted, c(1,2), function(did.vote) ifelse(did.vote==1, runif(1), -1*runif(1)))
#    mstar.2 <- apply(m.voted, c(1,2), function(did.vote) ifelse(did.vote==1, runif(1), -1*runif(1)))
#    abstain.jags.parameters = c("theta", "alpha", "beta", "delta", "gamma")
#    abstain.jags.inits.1 <- dump.format(list(zstar = abind(ystar.1, mstar.1, along=3), theta = rnorm(n.legislators), alpha = rnorm(n.item),
#      beta = rnorm(n.item), delta = rnorm(1), gamma = rnorm(1) ))
#    abstain.jags.inits.2 <- dump.format(list(zstar = abind(ystar.1, mstar.1, along=3), theta = rnorm(n.legislators), alpha = rnorm(n.item),
#      beta = rnorm(n.item), delta = rnorm(1), gamma = rnorm(1) ))
#    abstain.jags.data <- dump.format(list(rc=rc, m.voted=m.voted, lead=lead, party.membership=party.membership, n.legislators=n.legislators, n.item=n.item, 
#      lower.y=lower.y, upper.y=upper.y, lower.m=lower.m, upper.m=upper.m))
    
  } else {
    stop("No other mechanisms configured.")
  }
  
  jags.result.cjr <- run.jags (
    model=models$cjr[[this.ab.mech]],
    monitor=cjr.jags.parameters, n.chains=2,
    data=cjr.jags.data,
    inits=c(cjr.jags.inits.1, cjr.jags.inits.2),
    thin=10, burnin=5000, sample=15000,
    check.conv=TRUE, plots=FALSE)
  jags.result.abstain <- run.jags(
    model=models$abstain[[this.ab.mech]],
    monitor=abstain.jags.parameters, n.chains=2,
    data=abstain.jags.data,
    inits=c(abstain.jags.inits.1, abstain.jags.inits.2),
    thin=10, burnin=15000, sample=50000,
    check.conv=TRUE, plots=FALSE)
  
  jags.cjr <- list(mcmc=jags.result.cjr$mcmc, psrf=jags.result.cjr$psrf$psrf)
  jags.abstain <- list(mcmc=jags.result.abstain$mcmc, psrf=jags.result.abstain$psrf$psrf)

  batch.status.good <- postObjects(
    filename=paste(this.ab.mech, "-leg", i.legis, "-rate", this.ab.rate, ".RData", sep=""), 
    save.to.URL=upload.results.to, 
    post.params=list(pw=get.save.workunit.pw, project="abstentions", workunit=as.character(i.workunit)), 
    jags.cjr, jags.abstain)
}
