\name{RelationStrengthSimilarity}
\alias{RelationStrengthSimilarity}
\docType{package}
\title{
Calculate the Relation Strength Similarity from one node to another in a network.
}
\description{
Simple function using Rcpp
}
\usage{
RelationStrengthSimilarity(adjacency.matrix, v.from, v.to, longest.path.length)
}
\examples{
\dontrun{
adjacency.matrix <- matrix(0, nrow=6, ncol=6)
i <- 1; j <- 2; adjacency.matrix[i,j] <- adjacency.matrix[j,i] <- 1
i <- 2; j <- 3; adjacency.matrix[i,j] <- adjacency.matrix[j,i] <- 1
i <- 3; j <- 4; adjacency.matrix[i,j] <- adjacency.matrix[j,i] <- 1
i <- 4; j <- 5; adjacency.matrix[i,j] <- adjacency.matrix[j,i] <- 1
i <- 5; j <- 6; adjacency.matrix[i,j] <- adjacency.matrix[j,i] <- 1
i <- 6; j <- 1; adjacency.matrix[i,j] <- adjacency.matrix[j,i] <- 1
i <- 1; j <- 4; adjacency.matrix[i,j] <- adjacency.matrix[j,i] <- 1

RelationStrengthSimilarity(adjacency.matrix, 1, 3, 2)
RelationStrengthSimilarity(adjacency.matrix, 1, 3, 3)
RelationStrengthSimilarity(adjacency.matrix, 1, 3, 1)
}
}
