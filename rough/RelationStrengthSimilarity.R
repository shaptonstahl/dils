#' Relation Strength Similarity
#' 
#' Based on Chen et al (2012) "Discovering Missing Links in Networks 
#' Using Vertex Similarity Measures"

source("http://www.haptonstahl.org/R/Decruft/Decruft.R")  # creates a clean R session

source("rough//functions_RSS.R")

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

#' Equation 3

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


GetPathsAtoBRadius(M.test.1, v1=1, v2=2, radius=5)


GetPathsAtoBUnderRadius(M.test.1, v1=1, v2=2, radius=5)
GetPathsAtoBUnderRadius(M.test.1, v1=1, v2=2, radius=3)
GetPathsAtoBUnderRadius(M.test.1, v1=1, v2=2, radius=2)


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
