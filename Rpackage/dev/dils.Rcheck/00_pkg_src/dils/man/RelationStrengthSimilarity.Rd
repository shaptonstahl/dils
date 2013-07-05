\name{RelationStrengthSimilarity}
\alias{RelationStrengthSimilarity}
\title{Calculate the RSS from one node to another}
\usage{
  RelationStrengthSimilarity(xadj, v1, v2, radius)
}
\arguments{
  \item{xadj}{numeric matrix, then description of
  \code{arg1}.}

  \item{v1}{numeric Object type, then description of
  \code{arg2}.}

  \item{v2}{numeric Object type, then description of
  \code{arg2}.}

  \item{radius}{numeric, length of longest path examined
  from \code{v1} to \code{v2}.}
}
\value{
  numeric, the Relation Strength Similarity score from
  \code{v1} to \code{v2}.
}
\description{
  A longer description of the function.  This can be
  perhaps a paragraph, perhaps more than one.
}
\examples{
M.test.1 <- matrix(0, nrow=6, ncol=6)
M.test.1[1,2] <- M.test.1[2,1] <- 1
M.test.1[1,3] <- M.test.1[3,1] <- 1
M.test.1[3,4] <- M.test.1[4,3] <- 1
M.test.1[4,5] <- M.test.1[5,4] <- 1
M.test.1[5,6] <- M.test.1[6,5] <- 1
M.test.1[6,1] <- M.test.1[1,6] <- 1
M.test.1[1,4] <- M.test.1[4,1] <- 1
M.test.1
\dontrun{RelationStrengthSimilarity(xadj=M.test.1, v1=5, v2=6, radius=2)}
\dontrun{RelationStrengthSimilarity(xadj=M.test.1, v1=5, v2=6, radius=3)}
\dontrun{RelationStrengthSimilarity(xadj=M.test.1, v1=5, v2=6, radius=4)}
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
  \code{\link{ScalablePCA}}
}

