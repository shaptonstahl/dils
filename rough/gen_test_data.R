#' Generate test data for DILS

library(igraph)

n <- 50  #' multiple of 10. Number of nodes in each network

#' May want to add things from graph.atlas

nets <- list(ba.game(n=n, directed=FALSE),
             erdos.renyi.game(n=n, p.or.m=.8),
             grg.game(nodes=n, radius=.3, torus=TRUE),
             growing.random.game(n=n, m=3, directed=FALSE),
             interconnected.islands.game(islands.n=10,
                                         islands.size=n/10,
                                         islands.pin=.8,
                                         n.inter=n/10),
             k.regular.game (no.of.nodes=n, k=10)
)

ids <- sample(1:(2*n), n, replace=FALSE)
for(i in 1:length(nets)) {
  V(nets[[i]])$name <- sample(ids, n, replace=FALSE)  #' Name the nodes
  if( 1 == i %% 2 ) {
    #' Make about half the networks continuous
    E(nets[[i]])$weight <- rnorm(ecount(nets[[i]]))  
  } else {
    E(nets[[i]])$weight <- 1
  }
}

#' Plot the graphs to make sure they do not all look the same
plot(nets[[1]])
plot(nets[[2]])
plot(nets[[3]])
plot(nets[[4]])
plot(nets[[5]])
plot(nets[[6]])

E(nets[[1]])$weight
E(nets[[2]])$weight
E(nets[[3]])$weight
E(nets[[4]])$weight
E(nets[[5]])$weight
E(nets[[6]])$weight

saveRDS(nets, "rough/data_artificial.rds")