#' Generate DILS coefficients using SVD. This gives the same
#' factors as a modified version of the the method used to initialize
#' NOMINATE and IRT via ideal.

GenerateDILSCoefficients <- function(x) {
  #' Given n x k matrix 'x' of n observations of k variables,
  #' returns column vector 'v' of length k such that 
  #' 
  #'   x %*% v = s
  #'   
  #' where s is length-n column vector of scores of x projected along
  #' the first eigenvector. Polarity (which way is positive) is not
  #' specified and should be checked.
  #'
  #' Author: Stephen R. Haptonstahl (srh@haptonstahl.org)
  return( svd(x)$v[,1] )
}

#' testing scaling in R

GenerateTestMatrix <- function(n, k) {
  #' Generates an n x k matrix M with groups of columns that are
  #' correlated. This means that there will be a first dimension
  #' worth extracting.
  library(mvtnorm)
  if( k <= 2 ) { 
    return( matrix(rnorm(n * k), nrow=n) )
  } else {
    #' For k >= 3
    cov.matrix <- diag(k)
    i <- k
    start.col <- k - i + 1
    while(i > 0) {
      if( i >= 2 ) {
        new.cov <- .4
        if( start.col == 1 ) new.cov <- .8
        cov.matrix[start.col, start.col+1] <- new.cov
        cov.matrix[start.col+1, start.col] <- new.cov
      }
      i <- i - 2
      start.col <- k - i + 1
    }
    
    return( rmvnorm(n, sigma=cov.matrix) )
  }
}

k <- 10; n <- 100; test.x <- GenerateTestMatrix(n, k); system.time(test.coef <- GenerateDILSCoefficients(test.x)); test.coef
k <- 10; n <- 1e3; test.x <- GenerateTestMatrix(n, k); system.time(test.coef <- GenerateDILSCoefficients(test.x)); test.coef
k <- 10; n <- 1e4; test.x <- GenerateTestMatrix(n, k); system.time(test.coef <- GenerateDILSCoefficients(test.x)); test.coef

k <- 10; n <- 1e5; test.x <- GenerateTestMatrix(n, k); system.time(test.coef <- GenerateDILSCoefficients(test.x)); test.coef
k <- 100; n <- 1e5; test.x <- GenerateTestMatrix(n, k); system.time(test.coef <- GenerateDILSCoefficients(test.x)); test.coef
k <- 1e3; n <- 1e5; test.x <- GenerateTestMatrix(n, k); system.time(test.coef <- GenerateDILSCoefficients(test.x)); test.coef

k <- 1e3; n <- 1e3; test.x <- GenerateTestMatrix(n, k); system.time(test.coef <- GenerateDILSCoefficients(test.x)); test.coef

#' k <- 1e3; n <- 1e4  #' takes 29 seconds for GenerateDILSCoefficients
k <- 1e3; n <- 1e4; test.x <- GenerateTestMatrix(n, k); system.time(test.coef <- GenerateDILSCoefficients(test.x)); test.coef

test.scores <- test.x %*% as.matrix(test.coef)
hist(test.scores)
test.dils <- pnorm(test.scores)
hist(test.dils)

#' Check correlation with factor analysis results
k <- 1000
n <- 10000
test.x <- GenerateTestMatrix(n, k)
test.coef <- GenerateDILSCoefficients(test.x)
test.scores <- test.x %*% as.matrix(test.coef)

fa <- factanal(test.x, factors=1, scores="Bartlett")
cor(test.scores, fa$scores)
