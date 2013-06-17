#' Functions for calculating Relation Strength Similarity

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

GetPathsAtoBUnderRadius <- function(x.adj, v1, v2, radius) {
  out <- list()
  for(r in 1:radius) {
    out <- c(out, GetPathsAtoBRadius(x.adj, v1, v2, r))
  }
  return( out )
}

PathGeneralizedRelationStrength <- function(x.adj, path) {
  #' Given a path, return the Generalized Relation Strength (Equation 2)
  #' from the vertex at the beginning of the path to the vertex at the 
  #' vertex at the end.
  out <- 1
  for(i in 1:(length(path)-1)) {
    out <- out * x.adj[path[i], path[i+1]]
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
