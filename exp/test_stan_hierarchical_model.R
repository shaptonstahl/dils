#' Test fitting a hierarchical model

source("http://www.haptonstahl.org/R/Decruft/Decruft.R")

library(rstan)
set_cppo('fast')

#' #####  Generate fake data with known parameters to be recovered  #####
#' 
#' yi ~ N( beta xi + alphaj[i], sigma1 )
#' alphaj ~ N( gamma1 z1j + gamma2, sigma2 )
n.groups <- 10
n.obs <- 1000

true.gamma <- c(5,2)
true.sigma <- c(.2, .1)
true.beta <- 5
z <- rnorm(n.groups)
group.ids <- sample(1:n.groups, n.obs, replace=TRUE)
x <- rnorm(n.obs)
true.alpha <- rnorm(n=n.groups, mean=true.gamma[1] * z + true.gamma[2], sd=true.sigma[2])
y <- rnorm(n=n.obs, mean=true.beta * x + true.alpha[group.ids], sd=true.gamma[1])

#' data: x, y, z, group.ids
#' parameters: beta, alpha, gamma, sigma

#' #####  Set up and run STAN  #####
hier.model <- "
data {
  int<lower=0> n_obs;
  int<lower=0> n_groups;
  real x[n_obs];
  real y[n_obs];
  real z[n_groups];
  int<lower=0,upper=n_groups> group_ids[n_obs];
}
parameters {
  real alpha[n_groups];
  real beta;
  real gamma[2];
  real<lower=0> sigma[2];
}
model {
  for (j in 1:n_groups) {
    alpha[j] ~ normal(gamma[1] * z[j] + gamma[2], sigma[2]);
  }
  for (i in 1:n_obs) {
    y[i] ~ normal(alpha[group_ids[i]] + beta * x[i], sigma[1]);
  }
}
"
hier.data <- list(n_obs=n.obs,
                  n_groups=n.groups,
                  x=x,
                  y=y,
                  z=z,
                  group_ids=group.ids)
hier.pars <- c("alpha", "beta", "gamma", "sigma")

hier.fit.test1 <- stan(model_name="Test hierarchical regression",
                       model_code=hier.model,
                       data=hier.data,
                       pars=hier.pars,
                       chains=1,
                       iter=10,  # kept draws = (iter - warmup) / thin * chains
                       warmup=5,
                       thin=1,
                       verbose=TRUE,
                       save_dso=TRUE)
hier.fit.test2 <- stan(model_name="Test hierarchical regression",
                       fit=hier.fit.test1,
                       data=hier.data,
                       pars=hier.pars,
                       chains=2,
                       iter=100,  # kept draws = (iter - warmup) / thin * chains
                       warmup=10,
                       thin=2,
                       verbose=TRUE)
draws.test2 <- extract(hier.fit.test2)

hier.fit <- stan(model_name="Test hierarchical regression",
                 fit=hier.fit.test1,
                 data=hier.data,
                 pars=hier.pars,
                 chains=4,
                 iter=2000,  # kept draws = (iter - warmup) / thin * chains
                 warmup=500,
                 thin=2,
                 verbose=FALSE)

hier.fit <- hier.fit.test

#' #####  Compare the results  #####
draws <- extract(hier.fit)
str(draws)

fit.alpha <- colMeans(draws$alpha)
fit.beta <- mean(draws$beta)
fit.gamma <- colMeans(draws$gamma)
fit.sigma <- colMeans(draws$sigma)

true.alpha
fit.alpha

true.beta
fit.beta

true.gamma
fit.gamma

true.sigma
fit.sigma