#' Calculate the RSS from one node to another
#'
#' A longer description of the function.  This can be perhaps
#' a paragraph, perhaps more than one.
#' 
#' @param xadj numeric matrix, then description of \code{arg1}.
#' @param v1 numeric Object type, then description of \code{arg2}.
#' @param v2 numeric Object type, then description of \code{arg2}.
#' @param radius numeric, length of longest path examined from \code{v1} to \code{v2}.
#' @return numeric, the Relation Strength Similarity score from \code{v1} to \code{v2}.
#' @export
#' @seealso \code{\link{ScalablePCA}}
#' @references
#' "Discovering Missing Links in Networks Using Similarity Measures", 
#' Hung-Hsuan Chen, Liang Gou, Xiaolong (Luke) Zhang, C. Lee Giles. 2012.
#' @author Stephen R. Haptonstahl \email{srh@@haptonstahl.org}
#' @examples
#' M <- matrix(0, nrow=6, ncol=6)
#' M[1,2] <- M[2,1] <- 1
#' M[1,3] <- M[3,1] <- 1
#' M[3,4] <- M[4,3] <- 1
#' M[4,5] <- M[5,4] <- 1
#' M[5,6] <- M[6,5] <- 1
#' M[6,1] <- M[1,6] <- 1
#' M[1,4] <- M[4,1] <- 1
#' M
#' RelationStrengthSimilarity(xadj=M, v1=5, v2=6, radius=1)
#' RelationStrengthSimilarity(xadj=M, v1=5, v2=6, radius=2)
#' RelationStrengthSimilarity(xadj=M, v1=5, v2=6, radius=3)
#' RelationStrengthSimilarity(xadj=M, v1=5, v2=6, radius=4)
RelationStrengthSimilarity <- function(xadj, v1, v2, radius){
  #' Add guardians here
  stopifnot( is.matrix(xadj) )
  stopifnot( ncol(xadj) == nrow(xadj) )

  stopifnot(v1 %% 1 == 0)
  stopifnot(v1 >= 0)
  stopifnot( v1 <= nrow(xadj) )

  stopifnot(v2 %% 1 == 0)
  stopifnot(v2 >= 0)
  stopifnot( v2 <= nrow(xadj) )
  
  stopifnot(radius %% 1 == 0)
  stopifnot(radius > 0)
  
  RssThisRadius <- function(x, v1, v2, r, prepped=FALSE) {
    if( FALSE == prepped ) {
      diag(x) <- 0
      x <- sweep(x, 1, rowSums(x), "/")
    }
    n <- nrow(x)
    
    if( v1 == v2 ) {
      out <- 0
    } else if( 1 == r ) {
      out <- x[v1, v2]
    } else if( 2 == r ) {
      out <- sum(x[v1,] * x[,v2])
    } else if( 3 == r) {
      y <- sapply(1:n, function(ell) {
        RssThisRadius(x, v1, ell, 2, prepped=TRUE) - x[v1, v2] * x[v2, ell]
      })
      out <- sum(x[,v2] * y) + x[v1, v2] * x[v2, v1] * x[v1, v2]
    } else if( 4 == r ) {
      y <- sapply(1:n, function(ell) {
        RssThisRadius(x, v1, ell, 3, prepped=TRUE) - 
          x[v2, ell] * sum(x[v1,] * x[,v2]) +
          x[v2, ell] * x[v1, ell] * x[ell, v2] +
          x[v1, v2] * x[v2, v1] * x[v1, ell] -
          x[v1, v2] * sum(x[v2,] * x[,ell])
      })
      out <- sum(x[,v2] * y) + 
        x[v1,v2] * x[v2,v1] * sum(x[v1,] * x[,v2]) +
        x[v1,v2] * x[v1,v2] * sum(x[v2,] * x[,v1])
    } else {
      stop("RssThisRadius not yet supported for this value of r")
    }
    return( out )
  }
  
  diag(xadj) <- 0
  xadj <- sweep(xadj, 1, rowSums(xadj), "/")
  return( sum(sapply(1:radius, function(this.r) RssThisRadius(xadj, v1, v2, this.r, prepped=TRUE))))
}
