#' STAN estimation of regression, logit
#' 
source("http://www.haptonstahl.org/R/Decruft/Decruft.R")
library(rstan)

y <- readRDS("generated_y.rds")
true.parameters <- readRDS("generated_true_parameters.rds")


model.regression <- "
data {
  int<lower=0> N;
  real x[N];
  real y[N];
}
parameters {
  real alpha;
  real beta;
  real<lower=0> sigma;
}
model {
  for (n in 1:N)
    y[n] ~ normal(alpha + beta * x[n], sigma);
}
"
data.regression <- lapply(1:true.parameters$n.continuous.vars, function(k) 
  list(N=true.parameters$n.links,
       x=true.parameters$true.dils,
       y=y[,true.parameters$n.binary.vars+k])
)

fit.regression <- lapply(1:true.parameters$n.continuous.vars, function(k)
  stan(model_code=model.regression, data=data.regression[[k]])
)

print(fit.regression)
plot(fit.regression)


set_cppo(mode = c("debug"))
stan(model_code=model.regression, data=test.data)