\name{GetSampleFromDataFrame}
\alias{GetSampleFromDataFrame}
\title{Short description of the function}
\usage{
  GetSampleFromDataFrame(n, x)
}
\arguments{
  \item{n}{numeric, size of sample.}

  \item{x}{data.frame, data whose rows will be sampled.}
}
\value{
  data.frame, size n random subset of the rows of x
}
\description{
  A longer description of the function.  This can be
  perhaps a paragraph, perhaps more than one.
}
\examples{
data(iris)   # provides example data
x <- GetSampleFromDataFrame(10, iris)
}
\author{
  Stephen R. Haptonstahl \email{srh@haptonstahl.org}
}
\references{
  \url{http://www.haptonstahl.org/R}
}
\seealso{
  \code{\link{ScalablePCA}}
}

