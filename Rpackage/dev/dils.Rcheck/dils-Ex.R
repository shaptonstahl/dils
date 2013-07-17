pkgname <- "dils"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('dils')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("AdjacencyFromEdgelist")
### * AdjacencyFromEdgelist

flush(stderr()); flush(stdout())

### Name: AdjacencyFromEdgelist
### Title: Convert an edgelist to an adjacency matrix
### Aliases: AdjacencyFromEdgelist

### ** Examples

edgelist <- cbind(expand.grid(letters[1:2], letters[1:2]), runif(4))
AdjacencyFromEdgelist(edgelist)



cleanEx()
nameEx("EdgelistFill")
### * EdgelistFill

flush(stderr()); flush(stdout())

### Name: EdgelistFill
### Title: Ensure an edgelist has all dyads and a column of weights.
### Aliases: EdgelistFill

### ** Examples

g <- erdos.renyi.game(10, 2/10)
EdgelistFill(get.edgelist(g))
EdgelistFill(get.edgelist(g), nodelist=1:10)

E(g)$weight <- runif(ecount(g))
el <- cbind(get.edgelist(g), E(g)$weight)
EdgelistFill(el)
EdgelistFill(el, nodelist=1:10)



cleanEx()
nameEx("EdgelistFromAdjacency")
### * EdgelistFromAdjacency

flush(stderr()); flush(stdout())

### Name: EdgelistFromAdjacency
### Title: Convert an adjacency matrix to filled edgelist.
### Aliases: EdgelistFromAdjacency

### ** Examples

n <- 10
A <- matrix(rnorm(n*n), nrow=n)
A
EdgelistFromAdjacency(A)

n <- 100
A <- matrix(rnorm(n*n), nrow=n)
A
EdgelistFromAdjacency(A)

n <- 500
A <- matrix(rnorm(n*n), nrow=n)
A
## Not run: EdgelistFromAdjacency(A)



cleanEx()
nameEx("EdgelistFromIgraph")
### * EdgelistFromIgraph

flush(stderr()); flush(stdout())

### Name: EdgelistFromIgraph
### Title: Convert an igraph to filled edgelist
### Aliases: EdgelistFromIgraph

### ** Examples

g <- erdos.renyi.game(10, 2/10)
EdgelistFromIgraph(g)

V(g)$name <- letters[1:vcount(g)]
EdgelistFromIgraph(g)

E(g)$weight <- runif(ecount(g))
EdgelistFromIgraph(g, useWeight=TRUE)



cleanEx()
nameEx("GenerateDilsNetwork")
### * GenerateDilsNetwork

flush(stderr()); flush(stdout())

### Name: GenerateDilsNetwork
### Title: Combine multiple networks into a single weighted network.
### Aliases: GenerateDilsNetwork

### ** Examples

data(iris)        # provides example data
GenerateDilsNetwork(iris, subsample=10, use.cols=1:4)
GenerateDilsNetwork(iris, subsample=10, ignore.cols=5)



cleanEx()
nameEx("GetSampleFromDataFrame")
### * GetSampleFromDataFrame

flush(stderr()); flush(stdout())

### Name: GetSampleFromDataFrame
### Title: Randomly select rows from a data.frame.
### Aliases: GetSampleFromDataFrame

### ** Examples

data(iris)   # provides example data
x <- dils:::GetSampleFromDataFrame(10, iris)



cleanEx()
nameEx("GetSampleFromDb")
### * GetSampleFromDb

flush(stderr()); flush(stdout())

### Name: GetSampleFromDb
### Title: Sample from the rows of a (possibly large) database table (NOT
###   IMPLEMENTED)
### Aliases: GetSampleFromDb

### ** Examples

## Not run: x <- dils:::GetSampleFromDb(10, my.db)



cleanEx()
nameEx("GetSampleFromFile")
### * GetSampleFromFile

flush(stderr()); flush(stdout())

### Name: GetSampleFromFile
### Title: Sample from the rows of a (possibly large) text file (NOT
###   IMPLEMENTED)
### Aliases: GetSampleFromFile

### ** Examples

## Not run: x <- dils:::GetSampleFromFile(10, 150, "folder/containing/data.txt")



cleanEx()
nameEx("IgraphFromEdgelist")
### * IgraphFromEdgelist

flush(stderr()); flush(stdout())

### Name: IgraphFromEdgelist
### Title: Convert an edgelist to an igraph
### Aliases: IgraphFromEdgelist

### ** Examples

edgelist <- cbind(expand.grid(letters[1:2], letters[1:2]), runif(4))
g <- IgraphFromEdgelist(edgelist)
get.edgelist(g)
E(g)$weight
plot(g, edge.width=5*E(g)$weight, edge.curved=TRUE)



cleanEx()
nameEx("MeasureNetworkInformation")
### * MeasureNetworkInformation

flush(stderr()); flush(stdout())

### Name: MeasureNetworkInformation
### Title: Measure informativeness of a network of a particualar network
###   measure.
### Aliases: MeasureNetworkInformation

### ** Examples

g.rand <- random.graph.game(100, 5/100)
m.rand <- MeasureNetworkInformation(g.rand)
m.rand

pf <- matrix( c(.8, .2, .3, .7), nr=2)
g.pref <- preference.game(100, 2, pref.matrix=pf)
m.pref <- MeasureNetworkInformation(g.pref)
m.pref

m.pref / m.rand  # Relative informativeness of this preference graph
                 # to this random graph with respect to betweenness



cleanEx()
nameEx("MergeEdgelists")
### * MergeEdgelists

flush(stderr()); flush(stdout())

### Name: MergeEdgelists
### Title: Combine edgelists into a single data.frame
### Aliases: MergeEdgelists

### ** Examples

edgelist1 <- data.frame(expand.grid(letters[1:2], letters[1:2]),
                        uniform=runif(4))
edgelist2 <- data.frame(v1=c("a", "a"), v2=c("a", "b"), manual=c(.3, .5))
MergeEdgelists(edgelist1, edgelist2)



cleanEx()
nameEx("RelationStrengthSimilarity")
### * RelationStrengthSimilarity

flush(stderr()); flush(stdout())

### Name: RelationStrengthSimilarity
### Title: Calculate the RSS from one node to another.
### Aliases: RelationStrengthSimilarity

### ** Examples

M <- as.matrix(get.adjacency(graph.atlas(128)))
M
RelationStrengthSimilarity(xadj=M, v1=5, v2=6, radius=1)
RelationStrengthSimilarity(xadj=M, v1=5, v2=6, radius=2)
RelationStrengthSimilarity(xadj=M, v1=5, v2=6, radius=3)
RelationStrengthSimilarity(xadj=M, v1=5, v2=6, radius=4)
RelationStrengthSimilarity(xadj=M, radius=2)
## Not run: RelationStrengthSimilarity(xadj=M, radius=3)



cleanEx()
nameEx("RssCell")
### * RssCell

flush(stderr()); flush(stdout())

### Name: RssCell
### Title: Calculate the RSS from one node to another.
### Aliases: RssCell

### ** Examples

M <- as.matrix(get.adjacency(graph.atlas(128)))
M
M <- sweep(M, 1, rowMeans(M), "/")
M
dils:::RssCell(xadj=M, v1=5, v2=6, radius=1)
dils:::RssCell(xadj=M, v1=5, v2=6, radius=2)
dils:::RssCell(xadj=M, v1=5, v2=6, radius=3)
dils:::RssCell(xadj=M, v1=5, v2=6, radius=4)



cleanEx()
nameEx("RssThisRadius")
### * RssThisRadius

flush(stderr()); flush(stdout())

### Name: RssThisRadius
### Title: Calculate part of the RSS from one node to another.
### Aliases: RssThisRadius

### ** Examples

M <- as.matrix(get.adjacency(graph.atlas(128)))
M
dils:::RssThisRadius(x=M, v1=5, v2=6, r=1)
dils:::RssThisRadius(x=M, v1=5, v2=6, r=2)
dils:::RssThisRadius(x=M, v1=5, v2=6, r=3)
dils:::RssThisRadius(x=M, v1=5, v2=6, r=4)



cleanEx()
nameEx("ScalablePCA")
### * ScalablePCA

flush(stderr()); flush(stdout())

### Name: ScalablePCA
### Title: Perform Principal Component Analysis on a large data set
### Aliases: ScalablePCA

### ** Examples

data(iris)        # provides example data
prcomp(iris[,1:4], center=FALSE, scale.=FALSE)$rotation[,1]
ScalablePCA(iris, subsample=10, use.cols=1:4)
ScalablePCA(iris, subsample=10, ignore.cols=5)



### * <FOOTER>
###
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
