# My IRT test

source("http://www.haptonstahl.org/R/Decruft/Decruft.R")

library(pscl)
source("http://www.haptonstahl.org/R/RollCallFunctions/RollCallFunctions.R")
library(rstan)

####################################
###  Assume true parameters and  ###
###  genereate obeserved data    ###
####################################
n.nodes <- 150
n.dyads <- n.nodes * (n.nodes+1) / 2
n.networks <- 10
true.dils <- runif(n.dyads)
true.net.betas <- runif(n.networks, min=-2, max=2)
true.net.alphas <- runif(n.networks, min=-1, max=1)
true.y.star <- matrix(.5, nrow=n.dyads, ncol=n.networks)
for(i in 1:n.dyads) {
  for(j in 1:n.networks) {
    true.y.star[i,j] <- true.net.betas[j] * true.dils[i] - true.net.alphas[j]
  }
}
true.y.star <- true.y.star + rnorm(n.dyads, sd=.1)

y <- ifelse(true.y.star > 0, 1, 0)
# barplot(table(y))


#' Add a line of 1s and a line of zeros to force identification so that 
#' more 1s implies greater link strength.
y <- rbind(1, 0, y)

#################################################
###  Estimate parameters given observed data  ###
###  using ideal                              ###
#################################################
rc <- rollcall(y, legis.names=c("peg 1", 
                                "peg 0", 
                                paste("dyad", 1:(nrow(y)-2))))
priors.ideal <- constrain.legis(rc,
                                x=list("peg 1"=1, "peg 0"=0),
                                d=1)
fit.ideal <- ideal(rc, 
                   priors=priors.ideal,
                   maxiter=1e5+500,
                   thin=100,
                   verbose=TRUE)
fit.ideal <- DropIdealLegislator(fit.ideal, c(1,2))

################################################
###  Compare true parameter values to those  ###
###  estimated by ideal                      ###
################################################
est.dils.ideal <- as.numeric(fit.ideal$xbar)
cor(est.dils.ideal, true.dils)

#'  If this doesn't work, check to make sure the conditional
#'  variance added with rnorm is not too large. Too much noise
#'  will spoil the signal-to-noise soup.




#################################################
###  Estimate parameters given observed data  ###
###  using stan, straightforward model        ###
#################################################
irt.model <- '
data {
  int<lower=1> N;              // number of dyads
  int<lower=1> M;              // number of networks
  int<lower=0,upper=1> y[N,M]; // outcome for observation i,j
}
parameters {
  real<lower=0, upper=1> dils[N];
  real alpha[M];
  real beta[M];
}
model {
  dils[1] ~ uniform(.999, 1);  // spike prior
  dils[2] ~ uniform(0, .001);  // spike prior
  for(i in 3:N) {
    dils[i] ~ uniform(0,1);  // uninformative prior
  }
  alpha ~ normal(0, 3);      // uninformative prior
  beta ~ normal(0, 3);       // uninformative prior
  for (i in 1:N) {
    for(j in 1:M) {
      y[i,j] ~ bernoulli(Phi(beta[j] * dils[i] - alpha[j]));
    }
  }
}
'

irt.model <- '
data {
  // Initialize variables for data here
  int<lower=1> N;              // number of dyads
  int<lower=1> M;              // number of networks
  int<lower=0,upper=1> y[N,M]; // outcome for observation i,j
}
parameters {
  // Initialize variables for parameters here
  real<lower=0, upper=1> dils[N];
  real alpha[M];
  real beta[M];
}
model {
  // Give parameters priors here (cannot nail with '<-')
  dils[1] ~ normal(.995, .001);  // spike prior
  dils[2] ~ normal(.005, .001);  // spike prior
  for(i in 3:N) {
    dils[i] ~ uniform(0,1);  // uninformative prior
  }
  alpha ~ normal(0, 3);      // uninformative prior
  beta ~ normal(0, 3);       // uninformative prior

  // Specify model DGP here
  for (i in 1:N) {
    for(j in 1:M) {
      y[i,j] ~ bernoulli(Phi(beta[j] * dils[i] - alpha[j]));
    }
  }
}
'
irt.data=list(N=nrow(y), M=ncol(y), y=y)
irt.inits <- function() {
  return( list(dils=c(.999,.001,runif(nrow(y)-2)),
               alpha=rnorm(ncol(y), 0, 3),
               beta=rnorm(ncol(y), 0, 3)) )
}

irt.inits <- function() {
  good.inits <- InitializeIdeals(rc, 
                                 anchors=c(1,2), 
                                 anchor.values=c(1,0), 
                                 d=1)
  return( list(dils=good.inits$ideal.points[,1], 
               alpha=rnorm(ncol(y), 0, 3),
               beta=rnorm(ncol(y), 0, 3)) )
}



fit.model <- stan(model_name="IRT 1d with Probit link by SRH",
                  model_code=irt.model,
                  data=irt.data,
                  pars=c("dils", "beta", "alpha"),
                  chains=4,
                  iter=100,
                  warmup=0,
                  thin=1,
                  init=irt.inits,
                  save_dso=TRUE)

fit.run <- stan(model_name="IRT 1d with Probit link by SRH",
                fit=fit.model,
                data=irt.data,
                pars=c("dils", "beta", "alpha"),
                chains=4,
                iter=1e5,
                warmup=2e4,
                thin=100,
                init=irt.inits,
                save_dso=TRUE)

#################################################
###  Estimate parameters given observed data  ###
###  using stan, latent model                 ###
#################################################
irt.latent.model <- '
data {
  // Initialize variables for data here
  int<lower=1> N;              // number of dyads
  int<lower=1> M;              // number of networks
  int<lower=0,upper=1> y[N,M]; // outcome for observation i,j
}
parameters {
  // Initialize variables for parameters here
  real<lower=0, upper=1> dils[N];
  real alpha[M];
  real beta[M];
  real<lower=-5,upper=5> ystar[N,M];
}
model {
  // Give parameters priors here (cannot nail with '<-')
  dils[1] ~ normal(.995, .001);  // spike prior
  dils[2] ~ normal(.005, .001);  // spike prior
  for(i in 3:N) {
    dils[i] ~ uniform(0,1);  // uninformative prior
  }
  alpha ~ normal(0, 3);      // uninformative prior
  beta ~ normal(0, 3);       // uninformative prior
  ystar ~ uniform(-2,2);     // light information prior

  // Specify model DGP here
  for (i in 1:N) {
    for(j in 1:M) {
      ystar[i,j] ~ normal(beta[j] * dils[i] - alpha[j], 1);
      dils[i,j] <- int_step(ystar[i,j];
    }
  }
}
'
irt.data=list(N=nrow(y), M=ncol(y), y=y)

irt.inits <- function() {
  good.inits <- InitializeIdeals(rc, 
                                 anchors=c(1,2), 
                                 anchor.values=c(1,0), 
                                 d=1)
  return( list(dils=good.inits$ideal.points[,1], 
               alpha=rnorm(ncol(y), 0, 3),
               beta=rnorm(ncol(y), 0, 3)) )
}
fit.latent.model <- stan(model_name="IRT 1d with Probit link by SRH",
                  model_code=irt.latent.model,
                  data=irt.data,
                  pars=c("dils", "beta", "alpha", "ystar"),
                  chains=2,
                  iter=100,
                  warmup=10,
                  thin=1,
                  #init=irt.inits,
                  save_dso=TRUE)

fit.latent.run <- stan(model_name="IRT 1d with Probit link by SRH",
                fit=irt.latent.model,
                data=irt.data,
                pars=c("dils", "beta", "alpha"),
                chains=4,
                iter=1e5,
                warmup=2e4,
                thin=100,
                init=irt.inits,
                save_dso=TRUE)


################################################
###  Compare true parameter values to those  ###
###  estimated by stan                       ###
################################################
