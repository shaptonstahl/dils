# Playing with factor analysis

v1 <- c(1,1,1,1,1,1,1,1,1,1,3,3,3,3,3,4,5,6)
v2 <- c(1,2,1,1,1,1,2,1,2,1,3,4,3,3,3,4,6,5)
v3 <- c(3,3,3,3,3,1,1,1,1,1,1,1,1,1,1,5,4,6)
v4 <- c(3,3,4,3,3,1,1,2,1,1,1,1,2,1,1,5,6,4)
v5 <- c(1,1,1,1,1,3,3,3,3,3,1,1,1,1,1,6,4,5)
v6 <- c(1,1,1,2,1,3,3,3,4,3,1,1,1,2,1,6,5,4)
m1 <- cbind(v1,v2,v3,v4,v5,v6)
cor(m1)
factanal(m1, factors = 3) # varimax is the default
factanal(m1, factors = 3, rotation = "promax")
# The following shows the g factor as PC1
prcomp(m1) # signs may depend on platform

## formula interface
fa <- factanal(m1, factors=1, scores="Bartlett")

pred.scores <- t( t(fa$loadings) %*% sweep(t(m1), 1, rowMeans(t(m1))) / as.vector(t(fa$loadings) %*% fa$loadings) )




Z.matrix <- matrix(as.vector(t(m1)), nrow=6)
F.matrix <- t(as.matrix(fa$scores))
A.matrix <- Z.matrix %*% t(F.matrix) %*% solve(F.matrix %*% t(F.matrix))

raw.pred.scores <- m1 %*% A.matrix
cor(raw.pred.scores, fa$scores)
pred.scores <- scale(raw.pred.scores)
pred.scores
cor(pred.scores, fa$scores)

#' ##############################
L.matrix <- matrix(as.vector(L.matrix), ncol=fa$factors)
pred.scores <- t(solve( t(L.matrix) %*% L.matrix, t(L.matrix) %*% (Z.matrix - rowMeans(Z.matrix))))
pred.scores
cor(pred.scores, fa$scores)

#'  Home-rolled using eigen
V.matrix <- svd(m1)$v[,1]
pred.scores.3 <- m1 %*% V.matrix
pred.scores.3
cor(pred.scores.3, fa$scores)

calc.coef.2 <- svd(m1)$u[1,]
cor(calc.coef.2, fa$loadings)
pred.scores.4 <- m1 %*% calc.coef.2
cor(pred.scores.4, fa$scores)
#' horrible

# FA is for unconstrained variables.  First transform the data to make it this way.  Use any inverse CDF (I use the quantile function for the normal distribution, qnorm in R) for variables that start out between 0 and 1; use any log function for positive variables (if zero can occur, add 1 first.)  Then do factor analysis.  The results will be for the transformed variables, use their inverses to recover the original ones, if needed.

#' Using modified method for initial values used in NOMINATE and ideal

GenerateAgreementMatrix <- function(x) {
  #' Author: Stephen R. Haptonstahl (srh@haptonstahl.org)
  
  #' normalize input
  x <- scale(x)
  
  #' initialize output
  out <- diag(0, nrow(x))
  
  #' generate bottom half, copy to top half
  for(i in 2:nrow(x)) {
    for(j in 1:(i-1)) {
      out[i,j] <- mean( - abs(x[i,] - x[j,]) )
      out[j,i] <- out[i,j]
    }
  }
  return(out)
}
#' test.x <- matrix(rnorm(9), nrow=3); test.x; GenerateAgreementMatrix(test.x)

DoubleCenterSqrdDist <- function(x) {
  #' Author: Stephen R. Haptonstahl (srh@haptonstahl.org)
  work <- GenerateAgreementMatrix(x)
  work <- (1 - work)^2
  out <- sweep(work, 1, rowMeans(work))
  out <- sweep(out, 2, colMeans(work))
  out <- sweep(out, c(1,2), -mean(work))
  return(out / -2)
}
pred.scores.2 <- as.matrix(as.vector(scale(diag(DoubleCenterSqrdDist(m1)))))
cor(pred.scores.2, fa$scores)

#' Knowing scores are a linear transform of the data
calc.coef <- solve( t(m1) %*% m1 ) %*% t(m1) %*% as.matrix(as.vector(fa$scores))
pred.scores.1 <- m1 %*% calc.coef
cor(pred.scores.1, fa$scores)

cor(pred.scores.1, pred.scores.2)
cor(pred.scores.1, pred.scores.3)
cor(pred.scores.2, pred.scores.3)
