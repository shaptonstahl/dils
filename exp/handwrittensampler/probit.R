# Probit
# Draws using conjugugate conditionals
# Greenberg, p. 125

library(mvtnorm)
library(truncnorm)

###  Fake data  ###
n <- 250
n.ind.vars <- 3
true.beta <- c(1, 1:n.ind.vars)
true.sigma <- 1
X <- matrix(rnorm(n * n.ind.vars), ncol=n.ind.vars)
true.y.star <- apply(cbind(1, X), 1, function(this.row) sum(this.row * true.beta)) +
  rnorm(n, sd=true.sigma)
y <- ifelse(true.y.star > 0, 1, 0)
summary(glm(y ~ X, family=binomial(link = "probit")))

###  functions  ###
BetaGivenYstar <- function(current.y.star, X, y, B0inv, b0) {
  B1 = solve( (t(X) %*% X) + B0inv )
  bbar <- as.vector(solve( (t(X) %*% X) + B0inv, 
                           t(X) %*% as.matrix(current.y.star) + B0inv %*% as.matrix(b0) ))
  return( as.vector(rmvnorm(n=1, mean=bbar, sigma=B1)) )
}

YstarGivenBeta <- function(current.beta, X, y, a0, d0) {
  nay.indices <- which(0==y)
  y.star.means <- X %*% as.matrix(current.beta)
  y.star.means[nay.indices] <- - y.star.means[nay.indices]
  
  draws <- rtruncnorm(n=length(y),
                      a=0,
                      mean=y.star.means)
  draws[nay.indices] <- - draws[nay.indices]
  return( draws )
}

SRHprobit <- function(y, X, 
                          b0, 
                          B0=diag(ncol(X)+1), 
                          burnin=500,
                          keep.draws=1000,
                          thin=1,
                          debug=FALSE)
{
  # number of draws made ~~ burnin + keep.draws * thin
  B0inv <- solve(B0)
  X <- cbind(1, X)
  if(missing(b0)) b0 <- rep(0, ncol(X))
  
  current.y.star <- rexp(length(y))
  current.y.star[0 == y] <- - current.y.star[0 == y]
  
  if(debug) browser()
  
  current.beta <- BetaGivenYstar(current.y.star=current.y.star,
                               X=X,
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