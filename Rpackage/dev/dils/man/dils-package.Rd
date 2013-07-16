\name{dils-package}
\alias{dils-package}
\docType{package}
\title{
Data-Informed Link Strength. Combine multiple-relationship networks into a single weighted network.
}
\description{
Combine multiple-relationship networks into a single weighted network.  The approach is similar to factor analysys in the that contribution from each constituent network varies so as to maximize the information gleaned from the multimetwork.  This implementation uses Principal Component Analysis calculated using 'prcomp' with bootstrap subsampling.
}
\details{
\tabular{ll}{
Package: \tab dils\cr
Type: \tab Package\cr
Version: \tab 0.4\cr
Date: \tab 2013-07-16\cr
License: \tab MIT + file LICENSE\cr
}
Start with a table (data.frame, tab-delimited file, database) where each row/record represents a link between two nodes (a dyad) in a directed or undirected network and each column represents a different relationship between the two nodes, ie. each column is a network. DILS combines these columns/networks into a single network that is a weighted sum of the constituent networks.  The resulting DILS network uses information from all of the constituent networks and contains more information than any of the constituent networks.  The output is a data.frame of DILS scores for each dyad, therefore is a single network ready for analysis using \pkg{igraph} or other social network analysis (SNA) tools.
}
\author{
Stephen R. Haptonstahl <srh@haptonstahl.org>
}
\references{
"Discovering Missing Links in Networks Using Similarity Measures", 
Hung-Hsuan Chen, Liang Gou, Xiaolong (Luke) Zhang, C. Lee Giles. 2012.
}
\keyword{ network }
