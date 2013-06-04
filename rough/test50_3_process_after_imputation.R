#' Test data set with 50 nodes, after imputation

source("http://www.haptonstahl.org/R/Decruft/Decruft.R")

source("rough/functions.R")

dm.table.imputed <- readRDS("rough/test50_dm_table_imputed.rds")

#' Scale the link observe & imputed weight values in the 
#' DyadMultiTable to make the final DILS weights interpretable.
dm.table.imputed.scaled <- cbind(dm.table.imputed[,c(1:2)], 
                                 scale(dm.table.imputed[,c(3:ncol(dm.table.imputed))]))

weight.draws <- t(sapply(1:1000, function(g) {
  EstimatePcaWeights(dmt=dm.table.imputed.scaled, n.subsample=500)
}))
#' str(weight.draws)

weight.est <- colMeans(weight.draws)
weight.est

test.g.dils <- graph.edgelist(el=as.matrix(dm.table.imputed.scaled[,1:2]), directed=FALSE)
E(test.g.dils)$weight <- GenerateDILS(dm.table.imputed.scaled, weight.est)
plot(test.g.dils)
