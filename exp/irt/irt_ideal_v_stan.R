# My IRT test

library(pscl)
source("http://www.haptonstahl.org/R/RollCallFunctions/RollCallFunctions.R")

####################################
###  Assume true parameters and  ###
###  genereate obeserved data    ###
####################################
n.nodes <- 50
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

#################################################
###  Estimate parameters given observed data  ###
###  using ideal                              ###
#################################################
library(pscl)
rc <- rollcall(y)
fit.ideal <- ideal(rc, maxiter=1e4, verbose=TRUE)
est.dils.ideal <- as.numeric(fit.ideal$xbar)

################################################
###  Compare true parameter values to those  ###
###  estimated by ideal                      ###
################################################
cor(est.dils.ideal, true.dils)

#'  If this doesn't work, check to make sure the conditional
#'  variance added with rnorm is not too large. Too much noise
#'  will spoil the signal-to-noise soup.




#################################################
###  Estimate parameters given observed data  ###
###  using stan                              ###
#################################################

################################################
###  Compare true parameter values to those  ###
###  estimated by stan                      ###
################################################
