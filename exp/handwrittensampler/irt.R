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
true.x <- c(true.x[known.min.max], true.x[-known.min.max])

true.y.star <- matrix(0, nrow=n, ncol=m)
for(i in 1:n) {
  for(j in 1:m) {
    true.y.star[i,j] <- true.beta[j] * true.x[i] - true.alpha[j] + rnorm(1)
  }
}
# Force pegs to have all 1s or 0s
true.y.star[1,] <- - rexp(m)
true.y.star[2,] <- rexp(m)

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
ItemBetaAlphaGivenYstarX <- function(item.current.y.star, current.x) {
  x.cons <- cbind(-1, current.x)
  
  if( rcond(t(x.cons) %*% x.cons) < 1e-10 ) {
    browser("Reciprocal condition number too small")
  }
  
  B1 = solve( t(x.cons) %*% x.cons )
  bbar <- as.vector(solve( (t(x.cons) %*% x.cons),
                           t(x.cons) %*% as.matrix(item.current.y.star)))
  draw <- as.vector(rmvnorm(n=1, mean=bbar, sigma=B1))
  return( list(alpha=draw[1], beta=draw[2]) )
}

ItemXGivenYstarBetaAlpha <- function(item.current.y.star, current.beta, current.alpha) {
  B1 = solve( t(current.beta) %*% as.matrix(current.beta) )
  bbar <- as.vector(solve( t(current.beta) %*% as.matrix(current.beta), 
                           t(current.beta) %*% 
                             as.matrix(item.current.y.star + current.alpha) ))
  return( as.vector(rmvnorm(n=1, mean=bbar, sigma=B1)) )
}

RescaleParameters <- function(current.x, current.beta, current.alpha) {
  #' Assumes lower peg is first x, upper peg is second x
  #' Assumes lower peg value is -1, upper peg value is second 1
  s.ratio <- 2 / (current.x[2] - current.x[1])
  out.beta <- current.beta * 2 * current.x[1] * 
    (current.x[2] - current.x[1])
  out.alpha <- current.alpha + current.beta * current.x[1]
  out.x <- sapply(current.x, function(this.x) (this.x - current.x[1]) * s.ratio - 1)
  return( list(x=out.x, beta=out.beta, alpha=out.alpha) )
}

ItemYstarGivenBetaAlphaX <- function(item.current.beta, item.current.alpha,
                                     current.x, item.y) {
  nay.indices <- which(0==item.y)
  y.star.means <- current.x %*% as.matrix(item.current.beta) - item.current.alpha
  y.star.means[nay.indices] <- - y.star.means[nay.indices]
  
  draws <- rtruncnorm(n=length(item.y),
                      a=0,
                      mean=y.star.means)
  draws[nay.indices] <- - draws[nay.indices]
  return( draws )
}

SRH1dIRT <- function(y,
                     peg.ids=c(1,2),
                     burnin=500,
                     keep.draws=1000,
                     thin=1,
                     lop=.025,
                     debug1=FALSE, debug2=FALSE, debug3=FALSE, debug4=FALSE)
{
  #' number of draws made ~~ burnin + keep.draws * thin
  #' b0 = 0
  #' B0 = improper flat prior
  #' Scaled between -1 and 1
  
  #' Drop columns with almost no variation
  drop.columns <- apply(y, 2, function(this.column) {
    return( ifelse(mean(this.column) < lop | mean(this.column) > 1 - lop, TRUE, FALSE) )
  })
  if( sum(drop.columns) > 0 ) {
    y <- y[,!drop.columns]
    cat("Dropped the collowing columns because they were lopsided:\n")
    print(which(drop.columns))
  }
  
  #' Force pegs into first and second rows
  y <- rbind(y[peg.ids,], y[-peg.ids,])
  #' From here on assumes that x[1] = -1 and x[2] = 1

  N <- nrow(y)
  M <- ncol(y)
  
  #' initialize x, y.star, beta, and alpha
  current.x <- c(-1,1, runif(nrow(y)-2))
  
  current.y.star <- rexp(length(y))
  current.y.star[0 == y] <- - current.y.star[0 == y]
  current.y.star <- matrix(current.y.star, ncol=M, nrow=N)
  
  current.beta <- rep(0, M)
  current.alpha <- rep(0, M)
  
  if(debug1) browser()
  
  if(burnin > 0) {
    for(i in 1:burnin) {

      for(j in 1:M) {
        ab <- ItemBetaAlphaGivenYstarX(item.current.y.star=current.y.star[,j], 
                                       current.x=current.x)
        current.alpha[j] <- ab$alpha
        current.beta[j] <- ab$beta
      }
      
      for(i in 1:N) {
        current.x[i] <- ItemXGivenYstarBetaAlpha(item.current.y.star=current.y.star[i,],
                                                 current.beta,
                                                 current.alpha)
      }
      rescaled.params <- RescaleParameters(current.x=current.x, 
                                           current.beta=current.beta,
                                           current.alpha=current.alpha)
      current.x <- rescaled.params$x
      current.alpha <- rescaled.params$alpha
      current.beta <- rescaled.params$beta
      
      for(j in 1:M) {
        if(debug2) browser()
        current.y.star[,j] <- ItemYstarGivenBetaAlphaX(item.current.beta=current.beta[j], 
                                                      item.current.alpha=current.alpha[j],
                                                      current.x=current.x, 
                                                      item.y=y[,j])
      }
      
    } #' End burnin sampling loop
  }
  
  if(debug3) browser()
  
  draws <- list(beta=matrix(0, nrow=keep.draws, ncol=M),
                alpha=matrix(0, nrow=keep.draws, ncol=M),
                x=matrix(0, nrow=keep.draws, ncol=N))
  
  for(g in 1:(keep.draws*thin)) {
    for(j in 1:M) {
      ab <- ItemBetaAlphaGivenYstarX(item.current.y.star=current.y.star[,j], 
                                     current.x=current.x)
      current.alpha[j] <- ab$alpha
      current.beta[j] <- ab$beta
    }
    
    for(i in 1:N) {
      current.x[i] <- ItemXGivenYstarBetaAlpha(item.current.y.star=current.y.star[i,],
                                               current.beta,
                                               current.alpha)
    }
    rescaled.params <- RescaleParameters(current.x=current.x, 
                                         current.beta=current.beta,
                                         current.alpha=current.alpha)
    current.x <- rescaled.params$x
    current.alpha <- rescaled.params$alpha
    current.beta <- rescaled.params$beta
    
    for(j in 1:M) {
      current.y.star[,j] <- ItemYstarGivenBetaAlphaX(item.current.beta=current.beta[j], 
                                                     item.current.alpha=current.alpha[j],
                                                     current.x=current.x, 
                                                     item.y=y[,j])
    }
    
    if(0 == g %% thin) {
      if(debug4) browser()
      
      draws$beta[i/thin,] <- current.beta
      draws$beta[i/thin,] <- current.beta
      draws$beta[i/thin,] <- current.beta
    }
  }
  
  return(draws) 
}

SRH1dIRT(y=y, burnin=10, keep.draws=10, debug1=TRUE)
SRH1dIRT(y=y, burnin=10, keep.draws=10, debug2=TRUE)
SRH1dIRT(y=y, burnin=10, keep.draws=10, debug3=TRUE)
SRH1dIRT(y=y, burnin=10, keep.draws=10, debug4=TRUE)
SRH1dIRT(y=y, burnin=10, keep.draws=10)
