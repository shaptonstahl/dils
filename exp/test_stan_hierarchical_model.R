#' Test fitting a hierarchical model

source("http://www.haptonstahl.org/R/Decruft/Decruft.R")

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
                  y=y
                  z=z,
                  group_ids=group.ids)

#' #####  Compare the results  #####