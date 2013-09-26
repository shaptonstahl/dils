

MeasureNetworkInformation2 <- function(g, 
                                       FUN=betweenness,
                                       remove.share=.2,
                                       perturbed.sample.size=500,
                                       bootstrap.sample.size=30,
                                       progress.bar=FALSE,
                                       ...) {
  # First I calculate how well the given network measures FUN by taking the
  # std dev of FUN across bootstrapped networks for each node, then taking the mean.
  #
  # Then I do the same for other perturbed networks that have the same degree
  # distribution as the original network.
  #
  # I calculate a z-score for the given network relative to the perturbed networks.
  # I coerce this into (0,1) by taking the std normal CDF. Note that greater sd
  # is less information, so I subtract from 1 to get the final information score.
  
  SampleIgraph <- function(g, r.s=remove.share) {
    n.edges <- ecount(g)
    edges.to.remove <- sample(1:n.edges, round(r.s * n.edges))
    out <- delete.edges(g, E(g)[edges.to.remove])
    return(out)
  }
  
  MeasureNetwork <- function(g2) {
    draws <- matrix(0, nrow=bootstrap.sample.size, ncol=vcount(g2))  # rows = samples, columns = nodes
    for(k in 1:bootstrap.sample.size) {
      g.boot <- SampleIgraph(g2)
      draws[k,] <- do.call(FUN, list(g.boot, ...))
    }
    sds <- apply(draws, 2, sd)
    return(mean(sds))
  }
  
  if(progress.bar) pb <- txtProgressBar(max=perturbed.sample.size + 1, style=2)
  
  score.g <- MeasureNetwork(g)
  if(progress.bar) setTxtProgressBar(pb, 1)
  
  degs <- degree(g)
  degs <- degs[0!=degs]  # remove disconnected nodes because degree.sequence.game(method="vl") objects to them
  scores.perturbed <- numeric(perturbed.sample.size)
  
  for(i in 1:perturbed.sample.size) {
#    perturbed.g <- degree.sequence.game(degs, method="vl")
    perturbed.g <- degree.sequence.game(degs, method="simple")
    scores.perturbed[i] <- MeasureNetwork(perturbed.g)
    if(progress.bar) setTxtProgressBar(pb, i + 1)
  }
  
  # info measure is z-score of scores, coerced into (0,1) by the std normal CDF, subtracted from 1 to make higher numbers go with smaller sd
  score <- pnorm((score.g - mean(scores.perturbed))/sd(scores.perturbed))
  
  if(progress.bar) close(pb)
  
  return(score)
}

g.rand <- random.graph.game(100, 5/100)
MeasureNetworkInformation2(g.rand, pert=50, boot=50, progress.bar=TRUE)

SampleIgraph <- function(g, r.s=remove.share) {
  n.edges <- ecount(g)
  edges.to.remove <- sample(1:n.edges, round(r.s * n.edges))
  out <- delete.edges(g, E(g)[edges.to.remove])
  return(out)
}

# library(snow)
# cl <- makeCluster(6)
# parLapply(...)
# stopCluster(cl)

net.density <- seq(.95, .05, -.05)
g.set <- list(SampleIgraph(graph.full(30), .05))
for(i in 2:length(net.density)) {
  g.prev <- g.set[[length(g.set)]]
  remove <- 1 - net.density[i]/graph.density(g.prev)
  g.set <- c(g.set, list(SampleIgraph(g.prev, remove)))
}
# library(snow)
# cl <- makeCluster(6)
# measured.info.betweenness <- parSapply(cl, g.set, function(this.g) {
measured.info.betweenness <- sapply(g.set, function(this.g) {
  return( MeasureNetworkInformation2(this.g, pert=500, boot=500, progress.bar=TRUE) )
})
# stopCluster(cl)
plot(measured.info.betweenness ~ net.density, type="l")

