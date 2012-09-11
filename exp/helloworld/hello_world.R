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


schools_code <- '
  data {
    int<lower=0> J; // number of schools 
    real y[J]; // estimated treatment effects
    real<lower=0> sigma[J]; // s.e. of effect estimates 
  } 
  parameters {
    real theta[J]; 
    real mu; 
    real<lower=0> tau; 
  } 
  model {
    theta ~ normal(mu, tau); 
    y ~ normal(theta, sigma);
  } 
'
schools_dat <- list(J = 8, 
                    y = c(28,  8, -3,  7, -1,  1, 18, 12),
                    sigma = c(15, 10, 16, 11,  9, 11, 10, 18))
fit <- stan(model_code = schools_code, data = schools_dat, 
            iter = 1000, n_chains = 4)

fit1 <- stan(file = 'exp/8schools.stan', data = schools_dat, 
             iter = 1000, n_chains = 4)

fit2 <- stan(fit = fit1, data = schools_dat, iter = 10000, n_chains = 4)

######################

y <- read.table('exp/rats.txt', header = TRUE)
x <- c(8, 15, 22, 29, 36)
rats_dat <- list(N = nrow(y), T = ncol(y), 
                 x = x, y = y, xbar = mean(x))
rats_fit <- stan(file = 'exp/rats.stan', data = rats_dat, verbose = FALSE)