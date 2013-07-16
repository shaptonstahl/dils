pkgname <- "dils"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('dils')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
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
### Title: Converts adjacency matrix to filled edgelist
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
### Title: Converts an igraph to filled edgelist
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
### Title: Short description of the function
### Aliases: GetSampleFromDataFrame

### ** Examples

data(iris)   # provides example data
x <- GetSampleFromDataFrame(10, iris)



cleanEx()
nameEx("GetSampleFromDb")
### * GetSampleFromDb

flush(stderr()); flush(stdout())

### Name: GetSampleFromDb
### Title: Sample from the rows of a (possibly large) database table
### Aliases: GetSampleFromDb

### ** Examples

## Not run: x <- GetSampleFromDb(10, my.db)



cleanEx()
nameEx("GetSampleFromFile")
### * GetSampleFromFile

flush(stderr()); flush(stdout())

### Name: GetSampleFromFile
### Title: Sample from the rows of a (possibly large) text file
### Aliases: GetSampleFromFile

### ** Examples

## Not run: x <- GetSampleFromFile(10, 150, "folder/containing/data.txt")



cleanEx()
nameEx("RelationStrengthSimilarity")
### * RelationStrengthSimilarity

flush(stderr()); flush(stdout())

### Name: RelationStrengthSimilarity
### Title: Calculate the RSS from one node to another
### Aliases: RelationStrengthSimilarity

### ** Examples

M <- matrix(0, nrow=6, ncol=6)
M[1,2] <- M[2,1] <- 1
M[2,3] <- M[3,2] <- 1
M[3,4] <- M[4,3] <- 1
M[4,5] <- M[5,4] <- 1
M[5,6] <- M[6,5] <- 1
M[6,1] <- M[1,6] <- 1
M[1,4] <- M[4,1] <- 1
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
### Title: Calculate the RSS from one node to another
### Aliases: RssCell

### ** Examples

M <- matrix(0, nrow=6, ncol=6)
M[1,2] <- M[2,1] <- 1
M[2,3] <- M[3,2] <- 1
M[3,4] <- M[4,3] <- 1
M[4,5] <- M[5,4] <- 1
M[5,6] <- M[6,5] <- 1
M[6,1] <- M[1,6] <- 1
M[1,4] <- M[4,1] <- 1
M
M <- sweep(M, 1, rowMeans(M), "/")
M
RssCell(xadj=M, v1=5, v2=6, radius=1)
RssCell(xadj=M, v1=5, v2=6, radius=2)
RssCell(xadj=M, v1=5, v2=6, radius=3)
RssCell(xadj=M, v1=5, v2=6, radius=4)



cleanEx()
nameEx("RssThisRadius")
### * RssThisRadius

flush(stderr()); flush(stdout())

### Name: RssThisRadius
### Title: Calculate part of the RSS from one node to another
### Aliases: RssThisRadius

### ** Examples

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
