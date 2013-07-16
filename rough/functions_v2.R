


AdjacencyToEdgelist <- function(A, 
                                labels=paste("node", 1:nrow(A), sep="")) {
  # Guardians
  stopifnot(nrow(A) == ncol(A),
            length(labels) == nrow(A),
            make.names(labels) == labels)
  
  n <- nrow(A)
  out <- expand.grid(labels, labels)
  out$weight <- 0
  names(out) <- c("fromnode", "tonode", "weight")
  for(j in 1:n) {
    out$weight[((j-1)*n+1):(j*n)] <- A[,j]
  }
  return(out)
}
# n <- 10; A <- matrix(rnorm(n*n), nrow=n); A; AdjacencyToEdgelist(A)
# n <- 100; A <- matrix(rnorm(n*n), nrow=n); A; AdjacencyToEdgelist(A)
# n <- 500; A <- matrix(rnorm(n*n), nrow=n); A; AdjacencyToEdgelist(A)

IgraphToEdgelist <- function(g, useWeight=FALSE) {
  if(useWeight) {
    A <- get.adjacency(g, attr="weight")
  } else {
    A <- get.adjacency(g)
  }
  if( is.null(V(g)$name) ) {
    node.labels <- paste("node", 1:vcount(g), sep="")
  } else {
    node.labels <- V(g)$name
  }
  out <- AdjacencyToEdgelist(A, labels=node.labels)
  return(out)
}
# g <- erdos.renyi.game(10, 2/10); IgraphToEdgelist(g)
# V(g)$name <- letters[1:vcount(g)]; IgraphToEdgelist(g)
# E(g)$weight <- runif(ecount(g)); IgraphToEdgelist(g, useWeight=TRUE)

FillEdgelist <- function(elist, fillBlanksWith=0, nodelist) {
  # Guardians
  if( (is(elist, "matrix") || is(elist, "data.frame")) && 2 == ncol(elist) ) {
    # elist has no weights; perhaps it was the result of get.edgelist(g)
    elist <- cbind(elist, 1)  # put 1s in for each edge
  } else if( (is(elist, "matrix") || is(elist, "data.frame")) && 3 == ncol(elist) ) {
    # elist has a third column, presumably with weights
    stopifnot( is.numeric(elist[,3]) )
  } else {
    stop("elist must have two or three columns")
  }
  
  if( missing(nodelist) ) {
    nodelist <- sort(union(unique(elist[,1]), unique(elist[,2])))
  } else {
    stopifnot(all(elist[,1] %in% nodelist),
              all(elist[,2] %in% nodelist))
    nodelist <- sort(nodelist)
  }
  
  out <- expand.grid(nodelist, nodelist)
  out <- data.frame(out[,2], out[,1], fillBlanksWith)  # ensures sorted by first column, then second column
  if( is.null(names(elist)) ) {
    names(out) <- c("fromnode", "tonode", "weight")
  } else {
    names(out) <- names(elist)
  }
  
  for(i in 1:nrow(elist)) {
    out[out[,1]==elist[i,1] & out[,2]==elist[i,2], 3] <- elist[i,3]
  }
  return( out )
}
# g <- erdos.renyi.game(10, 2/10)
# FillEdgelist(get.edgelist(g))
# FillEdgelist(get.edgelist(g), nodelist=1:10)
# E(g)$weight <- runif(ecount(g))
# el <- cbind(get.edgelist(g), E(g)$weight)
# FillEdgelist(el)
# FillEdgelist(el, nodelist=1:10)
