pkgname <- "dils"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('dils')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
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
M[1,3] <- M[3,1] <- 1
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
