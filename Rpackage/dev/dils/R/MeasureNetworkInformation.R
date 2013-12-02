#' Measure how much a network informs a particular network measure
#'
#' Given an \code{igraph} network, evaluate the measure, discretize, and 
#' caluculate the (discrete) entropy of the observed values.
#' 
#' This function can vary tremendously based on the network measure being
#' considered and the other parameters.  It is only recommended that this
#' be used for comparing the informativeness of two networks on
#' the same set of nodes, keeping all the parameters the same.
#' 
#' @param g igraph, graph to measure
#' @param FUN function, a function that takes an igraph and returns a value for each node in the network.
#' @param score.range numeric, length-2 vector giving minimum and maximum possible values of \code{FUN}.
#' @return numeric, entropy of discretized \code{FUN} across the network
#' @export
#' @references
#' \url{https://github.com/shaptonstahl/dils}
#' @author Stephen R. Haptonstahl \email{srh@@haptonstahl.org}
#' @details Here information is measured as the entropy of \code{scores=FUN(g)}
#' after discretizing \code{scores} into bins.  The number of bins is the larger of
#' 10 or \code{round(sqrt(length(scores)))}.
#'   
#' This measure appears to be sensitive to the choice of \code{FUN}.
#' @examples
#' g.rand <- random.graph.game(100, 5/100)
#' m.rand <- MeasureNetworkInformation(g.rand)
#' m.rand
#' 
#' pf <- matrix( c(.8, .2, .3, .7), nr=2)
#' g.pref <- preference.game(100, 2, pref.matrix=pf)
#' m.pref <- MeasureNetworkInformation(g.pref)
#' m.pref
#' 
#' m.pref / m.rand  # Relative informativeness of this preference graph
#'                  # to this random graph with respect to betweenness
#' \dontrun{
#' prob.of.link <- c(1:30)/100
#' mnis <- sapply(1:30, function(p) {
#'   reps <- replicate(1e2, MeasureNetworkInformation(random.graph.game(100, p/100)))
#'   return( mean(reps) )
#' })
#' plot(prob.of.link, mnis, 
#'      type="l",
#'      main="Network Information of random graphs",
#'      xlab="probability of link formation",
#'      ylab="information (entropy)")
#' mtext("with respect to eigenvector centrality", line=0.5)
#' abline(v=which.max(mnis)/100)}
MeasureNetworkInformation <- function(g, 
                                      FUN=function(x) evcent(x)$vector, 
                                      score.range=c(0,1)) {
  scores <- FUN(g)
  
  if( max(scores) == min(scores) ) return(0)
  
  scores.out.of.range <- sum(scores < score.range[1]) + sum(scores > score.range[2])
  if( scores.out.of.range > 0 ) {
    warning(round(scores.out.of.range/length(scores)*1000)/10, "% of scores outside of range indicated. Using observed range of values.")
    score.range <- range(scores)
  }
  freqs <- discretize(scores, 
                      numBins=max(c(10, round(sqrt(length(scores))))),
                      r=score.range)
  return( entropy(freqs) )
}