\name{RssThisRadius}
\alias{RssThisRadius}
\title{Calculate part of the RSS from one node to another}
\usage{
  RssThisRadius(x, v1, v2, r, prepped = FALSE)
}
\arguments{
  \item{x}{numeric matrix, adjacency matrix where the [i,j]
  entry gives the strength of the link from node i to node
  j.}

  \item{v1}{numeric, index of the 'from' node.}

  \item{v2}{numeric, index of the 'to' node.}

  \item{r}{numeric, length of paths examined from \code{v1}
  to \code{v2}.}

  \item{prepped}{logical, whether or not the adjacency
  matrix \code{x} has had zeros entered on the diagonal and
  each row divided by the row sum.}
}
\value{
  numeric, the part of the Relation Strength Similarity
  score from \code{v1} to \code{v2} contributed by paths of
  length \code{r}.
}
\description{
  This is a helper function for RelationStrengthSimilarity
  that returns the component of RSS contributed by paths of
  one particular length r.
}
\examples{
M <- matrix(0, nrow=6, ncol=6)
M[1,2] <- M[2,1] <- 1
M[2,3] <- M[3,2] <- 1
M[3,4] <- M[4,3] <- 1
M[4,5] <- M[5,4] <- 1
M[5,6] <- M[6,5] <- 1
M[6,1] <- M[1,6] <- 1
M[1,4] <- M[4,1] <- 1
M
RssThisRadius(x=M, v1=5, v2=6, r=1)
RssThisRadius(x=M, v1=5, v2=6, r=2)
RssThisRadius(x=M, v1=5, v2=6, r=3)
RssThisRadius(x=M, v1=5, v2=6, r=4)
}
\author{
  Stephen R. Haptonstahl \email{srh@haptonstahl.org}
}
\references{
  "Discovering Missing Links in Networks Using Similarity
  Measures", Hung-Hsuan Chen, Liang Gou, Xiaolong (Luke)
  Zhang, C. Lee Giles. 2012.
}
\seealso{
  \code{\link{RelationStrengthSimilarity}}
}

