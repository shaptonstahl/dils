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
#' 'scale' (mean zero, sd one) the values of the links within each graph so weights will be directly
#' interpretable.
#' 
#' EstimatePcaWeights: Sample from dyads, do PCA, return weights. Can do with MR but not really necessary.
#' 
#' Generate a bunch of estimates of the weights (100? 1000?) and store them.Take means to get point estimates.
#' 
#' GenerateRawDILS: Just a dot product and application of phi, but use this with MR (if necessary) to generate ALL DILS scores.
#' 
#' DilsToAdjacency: Given raw DILS output (edgelist) convert to an adjacency matrix with proper rownames and columnnames
#' 
#' Then apply RSS


#' #####  Testing Functions  #####
#' Just need some code to generate some graphs with different values.
#' Need some with discrete values and some with continuous values.

library(igraph)
library(FastImputation)

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

FillDyadTable <- function(x, directed=FALSE, zero.to.na=TRUE) {
  out <- expand.grid(ids, ids)
  for(i in 3:ncol(x)) out <- cbind(out, ifelse(zero.to.na, NA, 0))
  out <- cbind(out[,c(2,1)], out[,-c(1:2)])
  out <- as.data.frame(out)
  names(out) <- names(x)
  
  for(i in 1:nrow(x)) {
    out[out$v1==x$v1[i] & out$v2==x$v2[i], -c(1:2)] <- x[i,-c(1:2)]
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
                                 directed=FALSE, 
                                 zero.to.na=TRUE) {
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
  x1 <- FillDyadTable(x1, zero.to.na=zero.to.na)
  x2 <- FillDyadTable(x2, zero.to.na=zero.to.na)
  
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
#' xyy <- CombineTwoDyadTables(x1=xy, x2=y, names2="erdos2")
#' head(xyy)

DyadMultiTable <- function(x.list, 
                           wt.names=names(x.list), 
                           directed=FALSE, 
                           zero.to.na=TRUE) {
  if( 1 == length(x.list) ) return( x.list )
  if( 2 == length(x.list) ) {
    return( CombineTwoDyadTables(x.list[[1]],
                                 x.list[[2]],
                                 names1=names(x.list)[1],
                                 names2=names(x.list)[2],
                                 zero.to.na=zero.to.na) )
  } else {
    out <- CombineTwoDyadTables(x.list[[1]],
                                x.list[[2]],
                                names1=names(x.list)[1],
                                names2=names(x.list)[2],
                                zero.to.na=zero.to.na)
    for(i in 3:length(x.list)) {
      out <- CombineTwoDyadTables(out,
                                  x.list[[i]],
#                                  names1=names(out)[-c(1:2)],
                                  names2=names(x.list)[i],
                                  zero.to.na=zero.to.na)
    }
    return( out )
  }
}
#' x.list <- lapply(nets, IgraphToDyadTable)
#' dm.table <- DyadMultiTable(x.list)
#' str(dm.table)

#####
# ImputeDyad <- function(v1, v2, dmt, g.list, n=2000, directed=FALSE) {
#   #'   1. Sample the other dyads.
#   #'   2. Get weights of each dyad relative to the given dyad and rescale to sum to 1.
#   #'   3. Get weighted mean and weighted covariance of sample using cov.wt
#   #'   4. Use FastImputation to impute the missing data.  
#   #' Run over all dyads as a MapReduce job with no reducer, then combine into a DyadMultiTable.
#   n.dyads <- nrow(dmt)
#   
#   row.ids.to.sample <- 1:n.dyads
#   row.ids.to.sample <- row.ids.to.sample[!(dmt$v1==v1 & dmt$v2==v2)]
#   sample.dyads <- data.frame(id=sort(sample(row.ids.to.sample, size=n)))
#   sample.dyads$v3 <- dmt[sample.dyads$id,"v1"]
#   sample.dyads$v4 <- dmt[sample.dyads$id,"v2"]
#   sample.dyads$weight <- NA
#   
#   for(i in 1:nrow(sample.dyads)) {
#     v3 <- sample.dyads$v3[i]
#     v4 <- sample.dyads$v4[i]
#     d13 <- sapply(g.list, function(g) { 
#       vid1 <- which(V(g)$name == v1)
#       vid3 <- which(V(g)$name == v3)
#       return( as.vector(shortest.paths(g, v=vid1, to=vid3, weights=NA)) )
#     })
#     d14 <- sapply(g.list, function(g) { 
#       vid1 <- which(V(g)$name == v1)
#       vid4 <- which(V(g)$name == v4)
#       as.vector(shortest.paths(g, v=vid1, to=vid4, weights=NA))
#     })
#     d23 <- sapply(g.list, function(g) { 
#       vid2 <- which(V(g)$name == v2)
#       vid3 <- which(V(g)$name == v3)
#       as.vector(shortest.paths(g, v=vid2, to=vid3, weights=NA))
#     })
#     d24 <- sapply(g.list, function(g) { 
#       vid2 <- which(V(g)$name == v2)
#       vid4 <- which(V(g)$name == v4)
#       as.vector(shortest.paths(g, v=vid2, to=vid4, weights=NA))
#     })
#     d1 <- sum(1/(1+pmin(d13, d14)))
#     d2 <- sum(1/(1+pmin(d23, d24)))
#     sample.dyads$weight[i] <- d1 + d2 - 2
#   }
#   
#   # Rescale weights to sum to 1
#   sample.dyads$weight <- sample.dyads$weight / sum(sample.dyads$weight)
#   
#   browser()
#   
#   weighted.cov.result <- cov.wt(dmt[sample.dyads$id, -c(1:2)], wt=sample.dyads$weight)
#   
#   #' "Artificially generate result of TrainFastImputation
#   FastImputation.patterns <- list(FI.means=weighted.cov.result$center,
#                                   FI.covariance=weighted.cov.result$cov,
#                                   FI.constraints=lapply(1:length(g.list), function(x) list(lower=0, upper=1)),
#                                   FI.cols.bound.to.intervals=1:length(g.list),
#                                   FI.cols.bound.to.sets=integer(0) )
#   class(FastImputation.patterns) <- "FastImputationPatterns"
#   
#   return( FastImputation(x=dmt[dmt$v1==v1 & dmt$v2==v2, -c(1:2)],
#                          patterns=FastImputation.patterns,
#                          verbose=FALSE) )
# }
#####

EstimatePcaWeights <- function(dmt, n.subsample=1e4) {
  sample.columns <- 3:ncol(dmt)
  sample.rows <- sample(1:nrow(dmt), size=n.subsample, replace=FALSE)
  
  pca.result <- prcomp(dmt[sample.rows,sample.columns], center=FALSE, scale.=FALSE)
  
  weights <- pca.result$rotation[,1]
  names(weights) <- names(dmt)[sample.columns]
  
  return( weights )
}
#' EstimatePcaWeights(dmt=dm.table.imputed.scaled, n.subsample=500)

GenerateDILS <- function(dmt, weights) {
  raw.dils <- sapply(1:nrow(dmt), function(i) sum(dmt[i,3:ncol(dmt)] * weights))
  return( pnorm(raw.dils) )
}
#' test.dils <- GenerateDILS(dm.table.imputed.scaled, weight.est)
#' hist(test.dils, 50)
