g <- random.graph.game(100, 5/100)

E(g)

E(g)[243]

E(delete.edges(g, E(g)[243]))

SampleIgraph <- function(g, removeShare=.2) {
  n.edges <- ecount(g)
  edges.to.remove <- sample(1:n.edges, round(removeShare * n.edges))
  out <- delete.edges(g, E(g)[edges.to.remove])
  return(out)
}

NetworkNodeMeanSd <- function(g, 
                          FUN=betweenness,
                          removeShare=.2,
                          sample.size=100) {
  n.nodes <- vcount(g)
  draws <- matrix(0, nrow=sample.size, ncol=n.nodes)
  for(k in 1:sample.size) {
    this.g <- SampleIgraph(g, removeShare=removeShare)
    draws[k,] <- do.call(FUN, list(this.g))
  }
  return( mean(apply(draws, 2, sd)) )
}
# g1 <- random.graph.game(100, 5/100)
# g2 <- random.graph.game(100, 2/100)
# NetworkNodeMeanSd(g1)
# NetworkNodeMeanSd(g2)

nnms <- sapply(1:25, function(p) {
  g <- random.graph.game(100, p/100)
  return( NetworkNodeMeanSd(g) )
})
plot(1:25 / 100, nnms, type="l", 
     main="NetworkNodeMeanSd for Random Graphs",
     xlab="Probability of drawing an edge",
     ylab="NetworkNodeMeanSd")