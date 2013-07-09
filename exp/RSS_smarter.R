#' RSS done smarter but with limits on the search radius r

RssThisRadius <- function(x, v1, v2, r, prepped=FALSE) {
  if( FALSE == prepped ) {
    diag(x) <- 0
    x <- sweep(x, 1, rowSums(x), "/")
  }
  n <- nrow(x)
  
  if( v1 == v2 ) {
    out <- 0
  } else if( 1 == r ) {
    out <- x[v1, v2]
  } else if( 2 == r ) {
    out <- sum(x[v1,] * x[,v2])
  } else if( 3 == r) {
    y <- sapply(1:n, function(ell) {
      RssThisRadius(x, v1, ell, 2, prepped=TRUE) - x[v1, v2] * x[v2, ell]
    })
    out <- sum(x[,v2] * y) + x[v1, v2] * x[v2, v1] * x[v1, v2]
  } else if( 4 == r ) {
    y <- sapply(1:n, function(ell) {
      RssThisRadius(x, v1, ell, 3, prepped=TRUE) - 
        x[v2, ell] * sum(x[v1,] * x[,v2]) +
        x[v2, ell] * x[v1, ell] * x[ell, v2] +
        x[v1, v2] * x[v2, v1] * x[v1, ell] -
        x[v1, v2] * sum(x[v2,] * x[,ell])
    })
    out <- sum(x[,v2] * y) + 
      x[v1,v2] * x[v2,v1] * sum(x[v1,] * x[,v2]) +
      x[v1,v2] * x[v1,v2] * sum(x[v2,] * x[,v1])
  } else {
    stop("RssThisRadius not yet supported for this value of r")
  }
  return( out )
}

RSS <- function(x, v1, v2, r) {
  diag(x) <- 0
  x <- sweep(x, 1, rowSums(x), "/")
  return( sum(sapply(1:r, function(this.r) RssThisRadius(x, v1, v2, this.r, prepped=TRUE))))
}

library(igraph)
M <- as.matrix(get.adjacency(graph.atlas(128)))
diag(M) <- 0
M <- sweep(M, 1, rowSums(M), "/")

RssThisRadius(M, 5, 6, 1)
RssThisRadius(M, 5, 6, 2)
RssThisRadius(M, 5, 6, 3)
RssThisRadius(M, 5, 6, 4)

library(dils)
RelationStrengthSimilarity(M, 5, 6, 1)
RelationStrengthSimilarity(M, 5, 6, 2)
RelationStrengthSimilarity(M, 5, 6, 3)
RelationStrengthSimilarity(M, 5, 6, 4)

RSS(M, 5, 6, 1)
RSS(M, 5, 6, 2)
RSS(M, 5, 6, 3)
RSS(M, 5, 6, 4)

tx <- function(n) {
  out <- matrix(runif(n*n), nrow=n)
  diag(out) <- 0
  out <- sweep(out, 1, rowSums(out), "/")
  return(out)
}

RSS(tx(20), 1, 2, 1)
RSS(tx(20), 1, 2, 2)
RSS(tx(20), 1, 2, 3)
RSS(tx(20), 1, 2, 4)

RSS(tx(50), 1, 2, 1)
RSS(tx(50), 1, 2, 2)
RSS(tx(50), 1, 2, 3)
RSS(tx(50), 1, 2, 4)

RSS(tx(100), 1, 2, 1)
RSS(tx(100), 1, 2, 2)
RSS(tx(100), 1, 2, 3)
RSS(tx(100), 1, 2, 4)

RSS(tx(500), 1, 2, 1)
RSS(tx(500), 1, 2, 2)
RSS(tx(500), 1, 2, 3)
RSS(tx(500), 1, 2, 4)
