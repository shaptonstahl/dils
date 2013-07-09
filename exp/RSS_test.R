#' Test 'dils' package RSS function


library(dils)

tx10 <- matrix(runif(100), nrow=10)
tx10 <- sweep(tx10, 1, rowSums(tx10), "/")

RelationStrengthSimilarity(tx10, 1, 2, 1)
RelationStrengthSimilarity(tx10, 1, 2, 2)
RelationStrengthSimilarity(tx10, 1, 2, 3)
RelationStrengthSimilarity(tx10, 1, 2, 4)

tx <- function(n) {
  out <- matrix(runif(n*n), nrow=n)
  diag(out) <- 0
  out <- sweep(out, 1, rowSums(out), "/")
  return(out)
}

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
