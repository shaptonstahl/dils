\name{RelationStrengthSimilarity}
\alias{RelationStrengthSimilarity}
\title{Calculate the RSS from one node to another.}
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
  numeric, Relation Strength Similarity score(s).
}
\description{
  For a single pair of nodes, implement the RSS algorithm
  of Chen et al. (2012).
}
\details{
  If \code{v1} and \code{v2} are specified, this returns
  the RSS from \code{v1} to \code{v2}.  If not, it
  calculates the RSS scores for all dyads in the network.
}
\examples{
M <- as.matrix(get.adjacency(graph.atlas(128)))
M
RelationStrengthSimilarity(xadj=M, v1=5, v2=6, radius=1)
RelationStrengthSimilarity(xadj=M, v1=5, v2=6, radius=2)
RelationStrengthSimilarity(xadj=M, v1=5, v2=6, radius=3)
RelationStrengthSimilarity(xadj=M, v1=5, v2=6, radius=4)
RelationStrengthSimilarity(xadj=M, radius=2)
\dontrun{RelationStrengthSimilarity(xadj=M, radius=3)}
}
\author{
  Stephen R. Haptonstahl \email{srh@haptonstahl.org}
}
\references{
  "Discovering Missing Links in Networks Using Similarity
  Measures", Hung-Hsuan Chen, Liang Gou, Xiaolong (Luke)
  Zhang, C. Lee Giles. 2012.

  \url{https://github.com/shaptonstahl/}
}
\seealso{
  \code{\link{ScalablePCA}}
}

