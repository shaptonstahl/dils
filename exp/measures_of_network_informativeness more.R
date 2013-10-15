# Test of a version of measuring relative network information

library(igraph)
# library(plyr)

RelativeNetworkInformation <- function(g.ref,
                                       g.compare=gref,
                                       f=betweenness,
                                       share.to.remove=.05,
                                       n.trials=1000,
                                       verbose=TRUE) {
  #' Given a reference graph and a comparison graph, compare
  #' the mean squared bootstrapped error for a function where 
  #' the value given by the reference graph is assumed to be the 
  #' true value.
  
  if(verbose) cat("Calculating reference values")
  reference.values <- f(g.ref)
  
  SampleIgraph <- function(g, share.to.remove=share.to.remove) {
    n.edges <- ecount(g)
    edges.to.remove <- sample(1:n.edges, round(share.to.remove * n.edges))
    out <- delete.edges(g, E(g)[edges.to.remove])
    return(out)
  }
  
  cat("Calculating reference deviation\n")
  reference.deviation <- mean(sapply(1:n.trials, function(k) {
    cat("\r  Trial", k, "               ")
    g.boot <- SampleIgraph(g=g.ref, share.to.remove=share.to.remove)
    boot.values <- f(g.boot)
    return( mean((reference.values - boot.values)^2) )
  }))
  cat("\nCalculating comparison deviation\n")  
  compare.deviation <- mean(sapply(1:n.trials, function(k) {
    g.boot <- SampleIgraph(g=g.compare, share.to.remove=share.to.remove)
    boot.values <- f(g.boot)
    return( mean((reference.values - boot.values)^2) )
  }))
  
  return(list(ref.advantage=compare.deviation/reference.deviation,
              ref.deviation=reference.deviation,
              compare.deviation=compare.deviation))
}

g.05 <- random.graph.game(100, 5/100)
g.20 <- random.graph.game(100, 20/100)

RelativeNetworkInformation(g.05, g.20, n.trials=200)


