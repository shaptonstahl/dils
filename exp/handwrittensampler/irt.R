# IRT
# Draws using conjugugate conditionals
# Jackman, p. 457

source("http://www.haptonstahl.org/R/Decruft/Decruft.R")

library(mvtnorm)
library(truncnorm)
library(pscl)
source("http://www.haptonstahl.org/R/RollCallFunctions/RollCallFunctions.R")

###  Fake data  ###
n <- 250
m <- 20
true.beta <- runif(m, min=-2, max=2)
true.alpha <- runif(m, min=-1, max=1)
true.x <- PegMinMax(runif(n))
known.min.max <- c(which.min(true.x), which.max(true.x))
true.y.star <- matrix(0, nrow=n, ncol=m)
for(i in 1:n) {
  for(j in 1:m) {
    true.y.star[i,j] <- true.beta[j] * true.x[i] - true.alpha[j] + rnorm(1)
  }
}
y <- ifelse(true.y.star > 0, 1, 0)
# barplot(table(y))

rc <- rollcall(y, legis.names=c(paste("dyad", 1:nrow(y))))
pegs <- list(-1,1)
names(pegs) <- c(paste("dyad", known.min.max[1]),
                 paste("dyad", known.min.max[2]))
priors.ideal <- constrain.legis(rc,
                                x=pegs,
                                d=1)
fit.ideal <- ideal(rc, 
                   priors=priors.ideal,
                   maxiter=1e5+500,
                   thin=100,
                   verbose=TRUE)
fit.ideal <- DropIdealLegislator(fit.ideal, c(1,2))



###  functions  ###
RescaleParameters <- function(current.x, 
                              current.beta, 
                              current.alpha)
{
  #' Assumes lower peg is first x, upper peg is second x
  #' Assumes lower peg value is -1, upper peg value is second 1
  out.beta <- current.beta * 2 * current.x[1] * 
    (current.x[2] - current.x[1])
  out.alpha <- current.alpha + current.beta * current.x[1]
  out.x <- c(0, (current.x[2] - current.x[1]) * current(x)[-1])
  return(x=out.x, beta=out.beta, alpha=out.alpha)
}

ItemYstarGivenBetaAlphaX <- function(item.current.beta, item.current.alpha,
                                     current.X, item.y) {
  nay.indices <- which(0==item.y)
  y.star.means <- current.X %*% as.matrix(item.current.beta) - item.current.alpha
  y.star.means[nay.indices] <- - y.star.means[nay.indices]
  
  draws <- rtruncnorm(n=length(item.y),
                      a=0,
                      mean=y.star.means)
  draws[nay.indices] <- - draws[nay.indices]
  return( draws )
}

ItemBetaAlphaGivenYstarX <- function(item.current.y.star, current.X, item.y) {
  current.X.with.constant <- cbind(-1, current.X)
  B1 = solve( t(current.X.with.constant) %*% current.X.with.constant )
  bbar <- as.vector(solve( (t(current.X.with.constant) %*% current.X.with.constant),
                           t(current.X.with.constant) %*% as.matrix(current.y.star)))
  draw <- as.vector(rmvnorm(n=1, mean=bbar, sigma=B1))
  return( list(alpha=draw[1], beta=draw[2]) )
}

ItemXGivenYstarBetaAlpha <- function(item.current.y.star, current.beta, current.alpha, y) {
  B1 = solve( t(current.beta) %*% as.matrix(current.beta) )
  bbar <- as.vector(solve( t(current.beta) %*% as.matrix(current.beta), 
                           t(current.beta) %*% 
                             as.matrix(item.current.y.star * current.alpha) ))
  return( as.vector(rmvnorm(n=1, mean=bbar, sigma=B1)) )
}

SRH1dIRT <- function(y,
                     burnin=500,
                     keep.draws=1000,
                     thin=1,
                     debug=FALSE)
{
  #' number of draws made ~~ burnin + keep.draws * thin
  #' b0 = 0
  #' B0 = improper flat prior
  
  cbind(-1, X)
  if(missing(b0)) b0 <- rep(0, ncol(X))
  
  current.y.star <- rexp(length(y))
  current.y.star[0 == y] <- - current.y.star[0 == y]
  
  if(debug) browser()
  
  current.beta <- BetaGivenYstar(current.y.star=current.y.star,
                                 X=cbind(-1, X),
                                 y=y,
                                 B0inv=B0inv,
                                 b0=b0)
  
  if(burnin > 0) {
    for(i in 1:burnin) {
      current.y.star <- YstarGivenBeta(current.beta=current.beta,
                                       X=X,
                                       y=y,
                                       a0=a0,
                                       d0=d0)
      current.beta <- BetaGivenYstar(current.y.star=current.y.star,
                                     X=X,
                                     y=y,
                                     B0inv=B0inv,
                                     b0=b0)
    }
  }
  
  draws <- list(beta=matrix(0, nrow=keep.draws, ncol=ncol(X)))
  
  for(i in 1:(keep.draws*thin)) {
    current.y.star <- YstarGivenBeta(current.beta=current.beta,
                                     X=X,
                                     y=y,
                                     a0=a0,
                                     d0=d0)
    current.beta <- BetaGivenYstar(current.y.star=current.y.star,
                                   X=X,
                                   y=y,
                                   B0inv=B0inv,
                                   b0=b0)
    if(0 == i %% thin) {
      draws$beta[i/thin,] <- current.beta
    }
  }
  
  return(draws) 
}

SRHprobit(y=y, X=X, keep.draws=10)
SRHprobit(y=y, X=X, debug=TRUE)