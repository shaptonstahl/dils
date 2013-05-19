#' DILS rough code to get the papers written for the conferences

#' #####  DILS functions  #####
#' 
#' IgraphToDyadTable: Given an igraph object returns a table with unique rows all dyads for which there is information.
#' 
#' FillDyadTable(x, directed=FALSE): Given a dyad table returns a dyad table with all unique pairs of dyads, whether or not there is information.
#' 
#' CombineTwoDyadTables(x1, x2, directed=FALSE): Combines two dyad tables checking compatibility and pairing on dyads carefully.
#' 
#' DyadMultiTable(x.list, directed=FALSE): Combines many dyad tables checking compatibility and pairing on dyads carefully.
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

source("http://www.haptonstahl.org/R/Decruft/Decruft.R")

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
#' x <- IgraphToDyadTable(nets[[1]])

FillDyadTable <- function(x, directed=FALSE) {
  out <- expand.grid(ids, ids, 0)
  out <- out[,c(2,1,3)]
  out <- as.data.frame(out)
  names(out) <- names(x)
  
  for(i in 1:nrow(x)) {
    out[out$v1==x$v1[i] & out$v2==x$v2[i], "weight"] <- x$weight[i]
  }
  return( out )
}
#' x <- IgraphToDyadTable(nets[[1]])
#' y <- IgraphToDyadTable(nets[[2]])
#' FillDyadTable(x)
#' x.filled <- FillDyadTable(x)
#' y.filled <- FillDyadTable(y)
#' nrow(x)
#' nrow(x.filled)
#' nrow(x) == sum(x.filled$weight != 0)
#' head(x.filled)
#' head(y.filled)

CombineTwoDyadTables <- function(x1, x2, 
                                 names1=names(x1)[-c(1:2)], 
                                 names2=names(x2)[-c(1:2)], 
                                 directed=FALSE) {
  #' Check that names lists are the right length and distinct
  if( names1 == "weight" && names2 == "weight") {
    names1 <- "w1"
    names2 <- "w2"
  } else {
    if( ncol(x1) != 2 + length(names1) ) stop("length(names1) must equal the number of weight columns in x1")
    if( ncol(x2) != 2 + length(names2) ) stop("length(names2) must equal the number of weight columns in x2")
    if( 0 != length(intersect(names1, names2)) ) stop("names1 and names2 must be distinct")
  }
  
  #' Check that ids are the same set
  if( sort(unique(x1$v1)) != sort(unique(x1$v1)) ||
        sort(unique(x1$v1)) != sort(unique(x1$v1)) ||
        sort(unique(x1$v1)) != sort(unique(x1$v1)) ||
        sort(unique(x1$v1)) != sort(unique(x1$v1)) ) stop ("Sets of ids not consistent")
  
  #' Ensure that both are full; this ensures order matches
  x1 <- FillDyadTable(x1)
  x2 <- FillDyadTable(x2)
  
  #' cbind
  out <- cbind(x1, x2[,-c(1:2),])
  names(out) <- c("v1", "v2", names1, names2)
  return( out )
}
#' x <- IgraphToDyadTable(nets[[1]])
#' y <- IgraphToDyadTable(nets[[2]])
#' xy <- CombineTwoDyadTables(x1=x, x2=y)
#' head(xy)
#' xy <- CombineTwoDyadTables(x1=x, x2=y, names1="ba", names2="erdos")
#' head(xy)

DyadMultiTable <- function(x.list, wt.names=names(x.list), directed=FALSE) {
  if( 1 == length(x.list) ) return( x.list )
  if( 2 == length(x.list) ) {
    return( CombineTwoDyadTables(x.list[[1]],
                                 x.list[[2]],
                                 names1=names(x.list)[1],
                                 names2=names(x.list)[2]) )
  } else {
    out <- CombineTwoDyadTables(x.list[[1]],
                                x.list[[2]],
                                names1=names(x.list)[1],
                                names2=names(x.list)[2])
    for(i in 3:length(x.list)) {
      out <- CombineTwoDyadTables(out,
                                  x.list[[i]],
#                                  names1=names(out)[-c(1:2)],
                                  names2=names(x.list)[i])
    }
    return( out )
  }
}
#' x.list <- lapply(nets, IgraphToDyadTable)
#' dm.table <- DyadMultiTable(x.list)



#####  Run the Code  #####




#' Scale the weights
for(i in 1:k) {
  E(nets[[i]])$weight <- scale(E(nets[[i]])$weight)
}

get.edgelist