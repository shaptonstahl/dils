#' RSS done smarter but with limits on the search radius r

M <- as.matrix(get.adjacency(graph.atlas(128)))


RssThisRadius <- function(x, v1, v2, r, prepped=FALSE) {
  if( FALSE == prepped ) {
    diag(x) <- 0
    x <- sweep(x, MARGIN=1, STATS=rowSums(x), FUN="/")
  }
  n <- nrow(x)
  
  if( v1 == v2 ) {
    out <- 0 
  } else if( 1 == r ) {
    out <- x[v1, v2]
  } else if( 2 == r ) {
    out <- sum(x[v1,] * x[,v2])
  } else if( 3 == r ) {
    y <- sapply(1:n, function(m) RssThisRadius(x, v1, m, r=2, prepped=TRUE) - x[v1,v2]*x[v2,m])
    out <- sum (x[,v2] * y) - x[v1,v2] * x[v2,v1] * x[v1,v2]
  }else {
    stop("RssThisRadius function not yet defined for this value of r")
  }
  return( out )
}

RssThisRadius(M, 1, 2, 1)
RssThisRadius(M, 1, 2, 2)
RssThisRadius(M, 1, 2, 3)
