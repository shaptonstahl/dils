#' Short description of the function
#'
#' A longer description of the function.  This can be perhaps
#' a paragraph, perhaps more than one.
#' 
#' @param n numeric, size of sample.
#' @param x data.frame, data whose rows will be sampled.
#' @return data.frame, size n random subset of the rows of x
#' @export
#' @seealso \code{\link{ScalablePCA}}, \code{\link{GetSampleFromFile}}, \code{\link{GetSampleFromFile}}
#' @references
#' \url{http://www.haptonstahl.org/R}
#' @author Stephen R. Haptonstahl \email{srh@@haptonstahl.org}
#' @examples
#' data(iris)   # provides example data
#' x <- GetSampleFromDataFrame(10, iris)
GetSampleFromDataFrame <- function(n,
                                   x) {
  # Guardians
  if(length(n) > 1) {
    n <- n[1]
    warning("n has multiple values; using only the first value")
  }
  stopifnot(is.numeric(n),
            n > 0,
            0 == n %% 1)
  stopifnot(is.data.frame(x),
            nrow(x) > 0,
            nrow(x) >= n)
  
  # perform the function
  out <- x[sample(1:nrow(x), n),]
  
  # prepare and return the output
  return(out)
}