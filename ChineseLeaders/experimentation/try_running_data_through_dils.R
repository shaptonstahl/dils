
library(dils)

# setwd("~/GitHub/Berico-Technologies/network-analytics/ChineseLeaders")

chinese.edgelist <- readRDS("networks/chinese_all_networks_edgelist.rds")

chinese.dils <- GenerateDilsNetwork(chinese.edgelist,
                                    subsample=1e5,
                                    n.subsamples=1e3,
                                    ignore.cols=1:2, 
                                    progress.bar=TRUE)

dils.edgelist <- cbind(chinese.edgelist[,1:2], chinese.dils$dils)
dils.edgelist <- dils.edgelist[order(dils.edgelist[,1], dils.edgelist[,2]),]
n <- sqrt(nrow(dils.edgelist))
dils.adj <- matrix(0, nrow=n, ncol=n)
for(i in 1:n) {
  cat("Filling row", i, "\n")
  dils.adj[i,] <- dils.edgelist[((i-1)*n+1):(i*n),3]
}
dils.network <- graph.adjacency(dils.adj, mode="undirected", weighted=TRUE)

system.time( dils.evcent <- evcent(dils.network) )
hist(dils.evcent$vector, 2000, ylim=c(0,200), main="DILS Eigenvector Centrality")

net.info.dils <- MeasureNetworkInformation(dils.network, FUN=function(g) evcent$vector, sample.size=5, progress.bar=TRUE)