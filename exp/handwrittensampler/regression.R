# Regression
# Draws using conjugugate conditionals
# Greenberg, p. 111

library(mvtnorm)

###  Fake data  ###
n <- 250
n.ind.vars <- 3
true.beta <- c(1, 1:n.ind.vars)
true.sigma <- 1
X <- matrix(rnorm(n * n.ind.vars), ncol=n.ind.vars)
y <- apply(cbind(1, X), 1, function(this.row) sum(this.row * true.beta)) +
  rnorm(n, sd=true.sigma)
summary(lm(y ~ X))

###  functions  ###
BetaGivenSsq <- function(current.ssq, X, y, B0inv, b0) {
  B1 = solve( (t(X) %*% X) / current.ssq + B0inv )
  bbar <- as.vector(solve( (t(X) %*% X) / current.ssq + B0inv, 
                 t(X) %*% as.matrix(y) / current.ssq + B0inv %*% as.matrix(b0) ))
  return( as.vector(rmvnorm(n=1, mean=bbar, sigma=B1)) )
}

SsqGivenBeta <- function(current.beta, X, y, a0, d0) {
  a1 <- a0 + length(y)
  ymXb <- y - X %*% as.matrix(current.beta)
  d1 <- d0 + t(ymXb) %*% ymXb
  return( 1/rgamma(1, a1/2, 2/d1) )
}

SRHregression <- function(y, X, 
                          b0, 
                          B0=diag(ncol(X)+1), 
                          a0=1, 
                          d0=1,
                          burnin=500,
                          keep.draws=1000,
                          thin=1,
                          debug=FALSE)
{
  # number of draws made ~~ burnin + keep.draws * thin
  
  B0inv <- solve(B0)
  X <- cbind(1, X)
  if(missing(b0)) b0 <- rep(0, ncol(X))
  
  current.ssq <- rexp(1)
  
  if(debug) browser()
  
  current.beta <- BetaGivenSsq(current.ssq=current.ssq,
                               X=X,
                               y=y,
                               B0inv=B0inv,
                               b0=b0)
  
  if(burnin > 0) {
    for(i in 1:burnin) {
      current.ssq <- SigmaGivenBeta(current.beta=current.beta,
                                    X=X,
                                    y=y,
                                    a0=a0,
                                    d0=d0)
      current.beta <- BetaGivenSsq(current.ssq=current.ssq,
                                   X=X,
                                   y=y,
                                   B0inv=B0inv,
                                   b0=b0)
    }
  }
  
  draws <- list(ssq=rep(keep.draws), 
                beta=matrix(0, nrow=keep.draws, ncol=ncol(X)))
  
  for(i in 1:(keep.draws*thin)) {
    current.ssq <- SigmaGivenBeta(current.beta=current.beta,
                                  X=X,
                                  y=y,
                                  a0=a0,
                                  d0=d0)
    current.beta <- BetaGivenSsq(current.ssq=current.ssq,
                                 X=X,
                                 y=y,
                                 B0inv=B0inv,
                                 b0=b0)
    if(0 == i %% thin) {
      draws$ssq[i/thin] <- current.ssq
      draws$beta[i/thin,] <- current.beta
    }
  }
  
 return(draws) 
}

SRHregression(y=y, X=X, keep.draws=10)
SRHregression(y=y, X=X, debug=TRUE)