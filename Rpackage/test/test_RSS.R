#' Test 'dils' package RSS function


library(dils)

tx10 <- matrix(runif(100), nrow=10)

RelationStrengthSimilarity(tx10, 1, 2, 1)
RelationStrengthSimilarity(tx10, 1, 2, 2)
RelationStrengthSimilarity(tx10, 1, 2, 3)
RelationStrengthSimilarity(tx10, 1, 2, 4)

tx <- function(n) matrix(runif(n*n), nrow=n)

RelationStrengthSimilarity(tx(10), 1, 2, 1)
RelationStrengthSimilarity(tx(10), 1, 2, 2)
RelationStrengthSimilarity(tx(10), 1, 2, 3)
RelationStrengthSimilarity(tx(10), 1, 2, 4)

RelationStrengthSimilarity(tx(20), 1, 2, 1)
RelationStrengthSimilarity(tx(20), 1, 2, 2)
RelationStrengthSimilarity(tx(20), 1, 2, 3)
RelationStrengthSimilarity(tx(20), 1, 2, 4)

RelationStrengthSimilarity(tx(30), 1, 2, 1)
RelationStrengthSimilarity(tx(30), 1, 2, 2)
RelationStrengthSimilarity(tx(30), 1, 2, 3)
RelationStrengthSimilarity(tx(30), 1, 2, 4)

RelationStrengthSimilarity(tx(50), 1, 2, 1)
RelationStrengthSimilarity(tx(50), 1, 2, 2)
RelationStrengthSimilarity(tx(50), 1, 2, 3)
RelationStrengthSimilarity(tx(50), 1, 2, 4)
