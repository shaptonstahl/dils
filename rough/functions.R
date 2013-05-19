#' DILS rough code to get the papers written for the conferences

#' #####  DILS functions  #####
#' 
#' IgraphToDyadTable: Given an igraph object returns a table with unique rows all dyads for which there is information.
#' 
#' MissingDyads(x, directed=FALSE): Returns data.frame with dyad pairs missing from x.
#' 
#' FillDyadTable(x, directed=FALSE): Given a dyad table returns a dyad table with all unique pairs of dyads, whether or not there is information.
#' 
#' CombineTwoDyadTables(x1, x2, directed=FALSE): Combines two dyad tables checking compatibility and pairing on dyads carefully.
#' 
#' DyadMultiTable(..., , directed=FALSE): Combines many dyad tables checking compatibility and pairing on dyads carefully.
#' 
#' ImputeDyad(x, i1, i2, directed=FALSE)
#'   1. Sample the other dyads.
#'   2. Get weights of each dyad relative to the given dyad and rescale to sum to 1.
#'   3. Get weighted mean and weighted covariance of sample using cov.wt
#'   4. Use FastImputation to impute the missing data.  
#' Run over all dyads as a MapReduce job with no reducer, then combine into a DyadMultiTable.
#' 
#' 'scale' (mean zero, sd one) the values of the links within each graph so weights will be directly
#' interpretable.
#' 
#' EstimatePcaWeights: Sample from dyads, do PCA, return weights. Can do with MR but not really necessary.
#' 
#' Generate a bunch of estimates of the weights (100? 1000?) and store them.Take means to get point estimates.
#' 
#' GenerateDILS: Just a dot product, but use this with MR to generate ALL DILS scores.
#' 
#' dils(..., directed=FALSE): Given a set of igraphs return and igraph of the DILS scores


#' #####  Testing Functions  #####
#' Just need some code to generate some graphs with different values.
#' Need some with discrete values and some with continuous values.

library(igraph)
nets <- readRDS("rough/data_artificial.rds")
n <- vcount(nets[[1]])
k <- length(nets)
ids <- sort(V(nets[[1]])$name)

#' A DyadTable has three columns ("v1", "v2", "weight") with v1 and v2 sorted.

IgraphToDyadTable <- function(g) {
  out <- data.frame(get.edgelist(g), E(g)$weight)
  names(out) <- c("v1", "v2", "weight")
  rownames(out) <- NULL
  out[, c("v1", "v2")] <- t(apply(out[, c("v1", "v2")], 1, sort))
  return( out )
}
#' IgraphToDyadTable(nets[[1]])





#' Scale the weights
for(i in 1:k) {
  E(nets[[i]])$weight <- scale(E(nets[[i]])$weight)
}

get.edgelist