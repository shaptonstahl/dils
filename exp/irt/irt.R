# Test STAN with a simple little IRT model

# To install STAN, must have Rtools installed.
# options(repos = c(getOption("repos"), rstan = "http://wiki.stan.googlecode.com/git/R"))
# install.packages("rstan")

#####  Awesome decrufting  #####
source("http://www.haptonstahl.org/R/Decruft/Decruft.R")

library(rstan)

#####  Set options  #####
# options(stringsAsFactors = FALSE)
if( "BT-SHAPTONSTAL" == Sys.info()["nodename"] ) {
  setwd("C:/Users/shaptonstahl/Documents/GitHub/dils")
} else if( interactive() ) {
  new.wd <- rchoose.dir()
  if(!is.na(new.wd)) setwd(new.wd)  # Interactively set working directory
  rm(new.wd)
} else {
  stop("Must set working directory")
}

irt.model <- '
  data {
    int<lower=1> nLegislators;
    int<lower=1> nItems;
    int rc[nLegislators,nItems]; // votes: 1=aye, 0=nay
  }
  parameters {
    real beta[nItems];            // discrimination paramters
    real alpha[nItems];           // difficulty parameters
    real theta[nLegislators];     // ideal points
  }
  model {
    for(i in 1:nLegislators) {
      for(j in 1:nItems) {
	    real p;
		p <- Phi(beta[j]*theta[i] - alpha[j]);
        rc[i,j] ~ bernoulli(p);
      }
    }
    alpha ~ normal(0.0, 4.0);
    beta ~ normal(0.0, 4.0);
    theta[1] ~ normal(-2.0, 0.01);                  # spike leftmost legislator
    for(i in 2:(nLegislators-1)) theta[i] ~ normal(0.0, 4.0);
    theta[nLegislators] ~ normal(2.0, 0.01);       # spike rightmost legislator
  }
'

# some fake data
n.items <- 30
n.legislators <- 10
votes <- matrix(sample(0:1, n.items*n.legislators, TRUE), nrow=n.legislators)

irt.data <- list(n.legislators = nrow(votes),
                 n.item = ncol(votes),
                 rc=votes)
fit.test <- stan(model_code = irt.model, 
                 data = irt.data, 
                 iter = 10, n_chains = 2)

fit <- stan(fit.test,
            data = irt.data, 
            iter = 1000, n_chains = 4)
				 