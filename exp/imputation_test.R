TestGraphBinary <- function(n, p=.3, directed=FALSE) {
  if(directed) {
    xadj <- matrix(sample(c(0,1), n*n, replace=TRUE, prob=c(1-p, p)), nrow=n)
    diag(xadj) <- 0
    return( graph.adjacency(xadj, mode="directed") )
  } else {
    xadj <- matrix(0, nrow=n, ncol=n)
    for(i in 1:n) {
      for(j in i:n) {
        xadj[i,j] <- xadj[j,i] <- sample(c(0,1), 1, prob=c(1-p, p))
      }
    }
    diag(xadj) <- 0
    return( graph.adjacency(xadj, mode="undirected") )
  }
}

plot(TestGraphBinary(5))
plot(TestGraphBinary(5, directed=TRUE))

g1 <- TestGraphBinary(100)
g1.imputation <- RssSuggestedNetwork(g1, q.impute.above=.25)
g1.imputation$frac.filled
plot(g1)
plot(g1.imputation$g.new)