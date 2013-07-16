#' Sample from the rows of a (possibly large) database table
#'
#' Accesses a dataabase table directly. Returns a data.frame whose rows are the sample.
#' 
#' @param n numeric, size of sample to be taken.
#' @param db connection, connection to the database table containing the data.
#' @return data.frame, size n random subset of the rows of filename
#' @export
#' @seealso \code{\link{ScalablePCA}}, \code{\link{GetSampleFromDataFrame}}, \code{\link{GetSampleFromFile}}
#' @references
#' \url{https://github.com/shaptonstahl/}
#' @author Stephen R. Haptonstahl \email{srh@@haptonstahl.org}
#' @examples
#' \dontrun{x <- GetSampleFromDb(10, my.db)}
GetSampleFromDb <- function(n, db) {
  # Guardians
  
  # determine the number of rows in the table
  
  # determine the rows to be sampled
  
  # query rows to be sampled
  
  # format for return
  
  return(NULL)
}