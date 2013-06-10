#' Relation Strength Similarity
#' 
#' Based on Chen et al (2012) "Discovering Missing Links in Networks 
#' Using Vertex Similarity Measures"

source("http://www.haptonstahl.org/R/Decruft/Decruft.R")  # creates a clean R session

library(igraph)

#' #####  Generate a matrix to try this on  #####
n <- 100
adj.dils <- matrix(runif(n*n), nrow=n)
adj.dils <- (adj.dils + t(adj.dils)) / 2
adj.dils[sample(1:(n*n), floor(n*n/2))] <- 0
diag(adj.dils) <- 1
adj.dils[1:8,1:8]

#' #####  Calculate RSS  #####

#' Equation 1
rel.str.adj <- sweep(adj.dils, 1, rowSums(adj.dils), "/")
rowSums(rel.str.adj)
rel.str.adj[1:8,1:8]

#' Turn into igraph
#' rel.str.g <- graph.adjacency(rel.str.adj, weighted=TRUE, diag=FALSE)
#' E(rel.str.g)$weight

#' Equation 3

GetNeighborIds <- function(x.adj, v.i, exclude) {
  #' Given an adjacency matrix x.adj and the index (column/row number) 
  #' v.i of a vertex return the paths of length 1 starting at v.i.
  #' 
  #' Note: If x.adj is directed it assumes weights go from row id to 
  #' column id.
  neighbors <- which(x.adj[v.i,] != 0)
  neighbors <- neighbors[neighbors != v.i]  #' remove self
  
  if( !missing(exclude) ) {
    for(id in exclude) {
      neighbors <- neighbors[neighbors != id]  #' excluded ids
    }
  }
  
  return(neighbors)
}

GetPathsLength1 <- function(x.adj, v.i, exclude) {
  #' Given an adjacency matrix x.adj and the index (column/row number) 
  #' v.i of a vertex return the paths of length 1 starting at v.i.
  #' 
  #' Note: If x.adj is directed it assumes weights go from row id to 
  #' column id.
  neighbors <- GetNeighborIds(x.adj=x.adj, v.i=v.i, exclude=exclude)
  
  out <- lapply(neighbors, function(target.id) {
    return( c(v.i, target.id) )
  })
  
  return( out )
}

GetMinPathsGivenRadius <- function(x.adj, v.i, radius=1, exclude=numeric(0)) {
  #' Recursive function on radius
  #' 
  #' Note: If x.adj is directed it assumes weights go from row id to 
  #' column id.
  
  if(1 == radius) {
    out <- GetPathsLength1(x.adj=x.adj, v.i=v.i, exclude=exclude)
  } else {
    out <- list()
    neighbors <- GetNeighborIds(x.adj=x.adj, v.i=v.i, exclude=exclude)
    for(n.i in neighbors) {
      next.paths <- GetMinPathsGivenRadius(x.adj=x.adj, 
                                           v.i=n.i, 
                                           radius=radius-1, 
                                           exclude=c(exclude,v.i))
      for(p in next.paths) {
        out <- c(out,  list(c(v.i, p)))
      }
    }
  }
  return( out )
}
#####
#'    2--3
#'   /    \
#'  1------4
#'   \    /
#'    6--5
M.test.1 <- matrix(0, nrow=6, ncol=6)
i <- 1; j <- 2; M.test.1[i,j] <- M.test.1[j,i] <- 1
i <- 2; j <- 3; M.test.1[i,j] <- M.test.1[j,i] <- 1
i <- 3; j <- 4; M.test.1[i,j] <- M.test.1[j,i] <- 1
i <- 4; j <- 5; M.test.1[i,j] <- M.test.1[j,i] <- 1
i <- 5; j <- 6; M.test.1[i,j] <- M.test.1[j,i] <- 1
i <- 6; j <- 1; M.test.1[i,j] <- M.test.1[j,i] <- 1
i <- 1; j <- 4; M.test.1[i,j] <- M.test.1[j,i] <- 1
M.test.1

GetMinPathsGivenRadius(M.test.1, v.i=1)
GetMinPathsGivenRadius(M.test.1, v.i=2)
GetMinPathsGivenRadius(M.test.1, v.i=1, radius=2)
GetMinPathsGivenRadius(M.test.1, v.i=1, radius=3)
GetMinPathsGivenRadius(M.test.1, v.i=1, radius=4)
GetMinPathsGivenRadius(M.test.1, v.i=1, radius=5)

GetPathsAtoBRadius <- function(x.adj, v1, v2, radius) {
  #' Given two vertices find all paths of length equal to the radius.
  all.paths <- GetMinPathsGivenRadius(x.adj, v.i=v1, radius=radius)
  out <- list()
  for(i in 1:length(all.paths)) {
    end.of.path <- all.paths[[i]][length(all.paths[[i]])]
    if( end.of.path == v2 ) {
      out <- c(out, all.paths[i])
    }
  }
  return( out )
}

GetPathsAtoBRadius(M.test.1, v1=1, v2=2, radius=5)

GetPathsAtoBUnderRadius <- function(x.adj, v1, v2, radius) {
  out <- list()
  for(r in 1:radius) {
    out <- c(out, GetPathsAtoBRadius(x.adj, v1, v2, r))
  }
  return( out )
}

GetPathsAtoBUnderRadius(M.test.1, v1=1, v2=2, radius=5)
GetPathsAtoBUnderRadius(M.test.1, v1=1, v2=2, radius=3)
GetPathsAtoBUnderRadius(M.test.1, v1=1, v2=2, radius=2)

PathGeneralizedRelationStrength <- function(rel.str.adj, path) {
  #' Given a path, return the Generalized Relation Strength (Equation 2)
  #' from the vertex at the beginning of the path to the vertex at the 
  #' vertex at the end.
  out <- 1
  for(i in 1:(length(path)-1)) {
    out <- out * rel.str.adj[path[i], path[i+1]]
  }
  return( out )
}

RelationStrengthSimilarity <- function(x.adj, v1, v2, radius) {
  #' Given two vertices and a radius returns RSS (Equation 4)
  #' from the first vertex to the second with a maximum degree 
  #' distance of radius.
  
  stopifnot( !missing(v1) && !missing(v2) )
  
  if(v1 == v2) {
    out <- 1
  } else {
    #' Return just the RSS from v1 to v2
    paths <- GetPathsAtoBUnderRadius(x.adj, v1=v1, v2=v2, radius=radius)
    if( 0 == length(paths) ) {
      out <- 0
    } else {
      out <- sum(sapply(paths, function(this.path) {
        PathGeneralizedRelationStrength(x.adj, this.path)
      }))
    }
  }         
  return( out )
}

RelationStrengthSimilarity(M.test.1, v1=1, v2=2, radius=5)
RelationStrengthSimilarity(M.test.1, v1=1, v2=2, radius=3)
RelationStrengthSimilarity(M.test.1, v1=1, v2=2, radius=2)
RelationStrengthSimilarity(M.test.1, v1=1, v2=1, radius=5)


M.RSS.1 <- matrix(NA, nrow=nrow(M.test.1), ncol=ncol(M.test.1))
for(i in 1:nrow(M.RSS.1)) {
  for(j in 1:ncol(M.RSS.1)) {
    M.RSS.1[i,j] <- RelationStrengthSimilarity(M.test.1, v1=i, v2=j, radius=3)
  }
}
M.RSS.1

M.test.2 <- M.test.1 * runif(nrow(M.test.1) * ncol(M.test.1))
M.test.2
M.RSS.2 <- matrix(NA, nrow=nrow(M.test.2), ncol=ncol(M.test.2))
for(i in 1:nrow(M.RSS.2)) {
  for(j in 1:ncol(M.RSS.2)) {
    M.RSS.2[i,j] <- RelationStrengthSimilarity(M.test.2, v1=i, v2=j, radius=2)
  }
}
M.RSS.2
