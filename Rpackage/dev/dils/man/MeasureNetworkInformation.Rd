\name{MeasureNetworkInformation}
\alias{MeasureNetworkInformation}
\title{Measure informativeness of a network of a particualar network measure.}
\usage{
  MeasureNetworkInformation(g, FUN = betweenness,
    remove.share = 0.2, sample.size = 100,
    progress.bar = FALSE)
}
\arguments{
  \item{g}{igraph, graph to measure}

  \item{FUN}{function, a function that takes an igraph and
  returns a value for each node in the network.}

  \item{remove.share}{numeric, fraction of the edges that
  are removed randomly when perturbing the network.}

  \item{sample.size}{numeric, number of perturbed graphs to
  generate}

  \item{progress.bar}{logical, if TRUE then a progress bar
  is shown.}
}
\value{
  numeric, mean precision of the measure \code{FUN} across
  the network
}
\description{
  Given an \code{igraph} network, repeatedly perturb the
  graph and take some measure of the network to see how
  much the measure varies.
}
\details{
  Here information is measured as 1 / mean across nodes of
  the standard deviation across perturbed graphs of a
  network node measure.

  Specifically, \code{sample.size} copies of the igraph are
  generated.  For each, \code{round(remove.share *
  n.edges)} randomly selected edges are dropped to generate
  a perturbed graph.  For each perturbed graph \code{FUN}
  is applied, generating a value for each node in the
  network.  For each node the standard deviation across the
  \code{sample.size} perturbed graphs is calculated,
  generating a standard deviation for each node. The mean
  is taken across the nodes in the network, and then
  reciprocated.

  This measure appears to be very sensitive to the choice
  of \code{FUN}.
}
\examples{
g.rand <- random.graph.game(100, 5/100)
m.rand <- MeasureNetworkInformation(g.rand)
m.rand

pf <- matrix( c(.8, .2, .3, .7), nr=2)
g.pref <- preference.game(100, 2, pref.matrix=pf)
m.pref <- MeasureNetworkInformation(g.pref)
m.pref

m.pref / m.rand  # Relative informativeness of this preference graph
                 # to this random graph with respect to betweenness
}
\author{
  Stephen R. Haptonstahl \email{srh@haptonstahl.org}
}
\references{
  \url{https://github.com/shaptonstahl/}
}

