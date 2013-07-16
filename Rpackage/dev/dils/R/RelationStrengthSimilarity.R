#' Calculate the RSS from one node to another
#'
#' A longer description of the function.  This can be perhaps
#' a paragraph, perhaps more than one.
#' 
#' @param xadj numeric matrix, then description of \code{arg1}.
#' @param v1 numeric Object type, then description of \code{arg2}.
#' @param v2 numeric Object type, then description of \code{arg2}.
#' @param radius numeric, length of longest path examined from \code{v1} to \code{v2}.
#' @return numeric, Relation Strength Similarity score(s).
#' @export
#' @seealso \code{\link{ScalablePCA}}
#' @references
#' "Discovering Missing Links in Networks Using Similarity Measures", 
#' Hung-Hsuan Chen, Liang Gou, Xiaolong (Luke) Zhang, C. Lee Giles. 2012.
#' 
#' \url{https://github.com/shaptonstahl/}
#' @author Stephen R. Haptonstahl \email{srh@@haptonstahl.org}
#' @details
#' If \code{v1} and \code{v2} are specified, this returns the RSS from \code{v1}
#' to \code{v2}.  If not, it calculates the RSS scores for all dyads in the network.
#' @examples
#' M <- matrix(0, nrow=6, ncol=6)
#' M[1,2] <- M[2,1] <- 1
#' M[2,3] <- M[3,2] <- 1
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
#' RelationStrengthSimilarity(xadj=M, radius=2)
#' \dontrun{RelationStrengthSimilarity(xadj=M, radius=3)}
RelationStrengthSimilarity <- function(xadj, v1, v2, radius){
  #' Add guardians here
  stopifnot( is.matrix(xadj) )
  stopifnot( ncol(xadj) == nrow(xadj) )
  # Prep the adjacency matrix
  diag(xadj) <- 0
  xadj <- sweep(xadj, 1, rowSums(xadj), "/")

  stopifnot(radius %% 1 == 0)
  stopifnot(radius > 0)
  
  if( missing(v1) ) {
    # calculate for the entire matrix
    # Check calculation time
    n <- nrow(xadj)
    if( (n*n)^radius > (20*20)^2 ) {
      cat("Estimating time to complete...\n")
      # Check a sample
      dyads <- expand.grid(1:n,1:n)
      test.dyad.rows <- sample(1:(n^2), 10)
      est.mean.time.seconds <- mean(sapply(1:10, function(k) {
        system.time(RssCell(xadj=xadj, 
                            v1=dyads[test.dyad.rows[k],1], 
                            v2=dyads[test.dyad.rows[k],2], 
                            radius=radius))[3]
      }))
      est.total.time.min <- round(n * n * est.mean.time.seconds / 60, 1)
      cat("This calculation should take", 
          est.total.time.min,
          "minutes on this computer.\n")
      ANSWER <- readline("Continue (y/n)? ")
      
      if( !("y" == ANSWER || "Y" == ANSWER) ) {
        cat("\n")
        return(invisible(NULL))
      } else {
        cat("Calculating...\n")
      }
    }
    
    #' Calculate
    out <- 0 * xadj
    if( all(xadj == t(xadj)) ) {
      # xadj is symmetric (is or is isomorphic to an undirected network)
      pb <- txtProgressBar(max=n*(n+1)/2, style=2)
      n.cells.complete <- 0
      for(i in 1:n) {
        for(j in i:n) {
          out[i,j] <- out [j,i] <- RssCell(xadj=xadj, v1=i, v2=j, radius=radius)
          n.cells.complete <- n.cells.complete + 1
          setTxtProgressBar(pb, n.cells.complete)
        }
      }
      close(pb)
    } else {
      # xadj is not symmetric (is a directed network)
      pb <- txtProgressBar(max=n*n, style=2)
      n.cells.complete <- 0
      for(i in 1:n) {
        for(j in 1:n) {
          out[i,j] <- RssCell(xadj=xadj, v1=i, v2=j, radius=radius)
          n.cells.complete <- n.cells.complete + 1
          setTxtProgressBar(pb, n.cells.complete)
        }
      }
      close(pb)
    }
    # END matrix calculation
  } else if( missing(v2) ) {
    stop("Must specify both v1 and v2 to calculate RSS for a single dyad")
  } else {
    #' calculate for a single dyad
    stopifnot(v1 %% 1 == 0)
    stopifnot(v1 >= 0)
    stopifnot( v1 <= nrow(xadj) )
    
    stopifnot(v2 %% 1 == 0)
    stopifnot(v2 >= 0)
    stopifnot( v2 <= nrow(xadj) )
    
    out <- RssCell(xadj=xadj, v1=v1, v2=v2, radius=radius)
  }
  return( out )
}