# functions

#####  Functions  #####
AddRows <- function(df, n) {
  # Adds rows to a data.frame filled with missing values
  new.rows <- as.data.frame(matrix(NA, nrow=n, ncol=ncol(df)))
  names(new.rows) <- names(df)
  return( rbind(df, new.rows) )
}

AnyBadIds <- function(subset, superset, return.bad.ids=FALSE) {
  bad.id.flags <- (0 == match(subset, superset, nomatch=1, incomparables=NA))
  if( any(bad.id.flags) ) {
    if(return.bad.ids) return( subset[bad.id.flags] )
    else return( TRUE )
  } else {
    return( FALSE )
  }
}

#####  capply  #####
#' applies a function to each column of a data.frame
#'
#' Applies a function to eaech column of a data.frame. If \code{class}
#' is not missing then will only convert columns whose class is among
#' those in \code{class}.
#' 
#' @param X data.frame.
#' @param FUN function to be applied to each column
#' @param class character variable with the name(s) of classes that should be converted
#' @return data.frame with FUN applied to each column
#' @export
#' @seealso \code{\link{DataFrameClasses}}
#' @references
#' Include papers, Web sites, etc.
#' \url{http://www.haptonstahl.org/R}
#' @author Stephen R. Haptonstahl \email{srh@@haptonstahl.org}
#' @examples
#' data(iris)   # provides example data
#' x <- Function(1, 2)     # Does something simple
#' x                       # Display result
#' 
#' y <- Function(1, 2, 3)  # Does something more complicated
#' y                       # Display result
capply <- function(X,
                   FUN,
                   target.classes) {
  # Guardians
  stopifnot(identical(class(X), "data.frame"),
            identical(class(FUN), "function"),
            ifelse(missing(target.classes), TRUE, is(target.classes, "character")))
  
  # deal with default and missing values
  if( missing(target.classes) ) {
    cols.to.coerce <- 1:ncol(X)
  } else {
    cols.to.coerce <- which(DataFrameClasses(X) %in% target.classes)
  }
  
  # perform the function
  for(i in cols.to.coerce) X[,i] <- FUN(X[,i])
  
  # prepare and return the output
  return(X)
}
#' Test code
#' test.df <- data.frame(w=c(1+2i, 3+4i, 5+6i), x=rnorm(3), y=sample(letters, 3), z=sample(c(TRUE, FALSE), 3, replace=TRUE))
#' test.df.as.character <- capply(test.df, as.character)
#' DataFrameClasses(test.df)
#' DataFrameClasses(test.df.as.character)
#' test.df
#' test.df.as.character
#' 
#' test.df.complex.to.double <- capply(test.df, Re, target.classes="complex")
#' DataFrameClasses(test.df.complex.to.double)
#' test.df.complex.to.double
#####

#####  ChildInstitutions  #####
ChildInstitutions <- function(id, url, db) {
  # Given id or url of an institution
  # returns vector of ids or urls of superset (url is a subset) institutions.
  #
  # Test: found.urls <- ChildInstitutions(url="http://www.chinavitae.com/institution/sta/3200.808", db=china)
  # Test: identical(china$institutions$url[china$institutions$id %in% ChildInstitutions(id=2312, db=china)], found.urls)
  if(missing(id)) {
    if( missing(url) ) stop("Must specify either id or url of the ego institution")
    rows.to.keep <- grep(pattern=paste(url, ".+", sep=""), x=db$all.institutions$url)
    return( db$all.institutions$url[rows.to.keep] )
  } else {
    url <- db$all.institutions$url[db$all.institutions$id==id]
    rows.to.keep <- grep(pattern=paste(url, ".+", sep=""), x=db$all.institutions$url)
    return( db$all.institutions$id[rows.to.keep] )
  }
}
#####

#####  ComplexToReal  #####
#' Converts complex coilumns of a data.frame to real
#'
#' A longer description of the function.  This can be perhaps
#' a paragraph, perhaps more than one.
#' 
#' @param X data.frame.
#' @return data.frame with each Complex column coerced into 
#' @export
#' @seealso \code{\link{Related function}}
#' @references
#' Include papers, Web sites, etc.
#' \url{http://www.haptonstahl.org/R}
#' @author Stephen R. Haptonstahl \email{srh@@haptonstahl.org}
#' @examples
#' data(iris)   # provides example data
#' x <- Function(1, 2)     # Does something simple
#' x                       # Display result
#' 
#' y <- Function(1, 2, 3)  # Does something more complicated
#' y                       # Display result
ComplexToReal <- function(X) return( capply(X, Re, "complex") )
#####

CreateClusterNetwork <- function(node.table, 
                                 cluster.on=names(node.table)[1],
                                 names.field="id") {
  # Given a data.frame for a source, a variable to cluster on, 
  # and a variable to provide node names, returns the adjacency
  # matrix for a network where clusters are full subnetworks.
  
  n <- nrow(node.table)
  adjacency.matrix <- matrix(0, nrow=n, ncol=n)
  cluster.ids <- split(x=node.table[,names.field], f=node.table[,cluster.on])
  for(i in length(cluster.ids)) {
    is.member <- (node.table[,names.field] %in% cluster.ids[[i]])
    adjacency.matrix[is.member, is.member] <- 1
  }
  diag(adjacency.matrix) <- 0
  rownames(adjacency.matrix) <- colnames(adjacency.matrix) <- node.table[,names.field]
  return( adjacency.matrix )
}


CreateDummyNetwork <- function(node.table, 
                               dummy.name,
                               dummy.linked.value=1,
                               names.field="id") {
  n <- nrow(node.table)
  adjacency.matrix <- matrix(0, nrow=n, ncol=n)
  dummy.var <- (node.table[,dummy.name] == dummy.linked.value)
  adjacency.matrix[dummy.var, dummy.var] <- 1
  diag(adjacency.matrix) <- 0
  rownames(adjacency.matrix) <- colnames(adjacency.matrix) <- node.table[,names.field]
  return( adjacency.matrix )
}

#####  DataFrameClasses  #####
#' Retrieve the class of each column in a data.frame
#'
#' A longer description of the function.  This can be perhaps
#' a paragraph, perhaps more than one.
#' 
#' @param X data.frame.
#' @return character vector with names of classes of each column of X
#' @export
#' @seealso \code{\link{Related function}}
#' @references
#' Include papers, Web sites, etc.
#' \url{http://www.haptonstahl.org/R}
#' @author Stephen R. Haptonstahl \email{srh@@haptonstahl.org}
#' @examples
#' data(iris)   # provides example data
#' DataFrameClasses(iris)
DataFrameClasses <- function(X) {
  # Guardians
  stopifnot(class(X) == "data.frame")
  
  # perform the function
  out <- sapply(1:ncol(X), function(i) class(X[,i]))
  
  # prepare and return the output
  return(out)
}
#' data(iris)   # provides example data
#' DataFrameClasses(iris)

#####
# ExtractLinks <- function(url) {
#   # Test: stack.links <- ExtractLinks("http://stackoverflow.com/questions/3746256/extract-links-from-webpage-using-r")
#   doc <- htmlParse(url)
#   links <- xpathSApply(doc, "//a/@href")
#   free(doc)
#   names(links) <- NULL
#   return(links)
# }
# 
#####

ExportAdjacencyMatrix <- function(m, loc.stem="./exported") {
  saveRDS(m, file=paste(loc.stem, "_network_adjacency.rds", sep=""))
  write.table(m, 
              file=paste(loc.stem, "_network_adjacency.tsv", sep=""),
              quote=FALSE,
              sep="\t",
              row.names=FALSE,
              col.names=FALSE)
  if( !is.null(colnames(m)) ) {
    write.table(colnames(m), 
                file=paste(loc.stem, "_network_names.txt", sep=""),
                quote=FALSE,
                sep="\t",
                row.names=FALSE,
                col.names=FALSE)
  }
  invisible(NULL)
}

ExtractLinksAndTitles <- function(url) {
  # Test: stack.links.and.titles <- ExtractLinksAndTitles("http://stackoverflow.com/questions/3746256/extract-links-from-webpage-using-r")
  doc <- htmlParse(url)
  urls <- xpathSApply(doc, "//a/@href")
  names(urls) <- NULL
  full.links <- xpathSApply(doc, "//a")
  free(doc)
  titles <- sapply(full.links, function(this.link.obj) xmlValue(this.link.obj))
  titles <- titles[sapply(full.links, function(x) any(names(xmlAttrs(x)) == "href"))]  # make sure we remove links that don't have href
  return( data.frame(url=urls, title=titles) )
}
 
#####
# ExtractLinkTitles <- function(url) {
#   # Test: stack.links <- ExtractLinkTitle("http://stackoverflow.com/questions/3746256/extract-links-from-webpage-using-r")
#   doc <- htmlParse(url)
#   full.links <- xpathSApply(doc, "//a")
#   free(doc)
#   link.titles <- sapply(full.links, function(this.link.obj) xmlValue(this.link.obj))
#   return(link.titles)
# }
#####

GetGUIDs <- function(n) {
  replicate(n, {
    doc <- readLines("http://www.guidgen.com/")
    the.line <- doc[grepl('<input name="YourGuidLabel"', doc)]
    guid <- sub("^.*value=\"([[:xdigit:]-]+)\".*$", "\\1", the.line)
    guid
  })
}

#####  HierarchicalKmeans  #####

#' Generate hierarchical clustering using k-means recursively
#'
#' Recursive function that starts with the entire set of observations
#' and repeatedly partitions the rows using \code{kmeans} until reaching
#' singleton rows.
#' 
#' @param X data.frame or matrix, rows are observations to be clustered, columns are attributes
#' @param k numeric, with first element coercable to an integer >= 2. Other elements are ignored. Number of clusters identified at each stage.
#' @param ids numeric, giving ids numbers to report back 
#' @return list, containing \code{ids} a vector listing the rows in the cluster and \code{subclusters} a list of clusters when this cluster is decomposed. Each cluster has the same format (vector of ids, list of clusters) recursively until singletons are reached, indicated by \code{subclusters=NULL}.
#' @export
#' @seealso \code{\link{Related function}}
#' @references
#' Method developed by Stephen R. Haptonstahl
#' \url{http://www.haptonstahl.org/R}
#' @author Stephen R. Haptonstahl \email{srh@@haptonstahl.org}
#' @examples
#' data(iris)   # provides example data
#' x <- Function(1, 2)     # Does something simple
#' x                       # Display result
#' 
#' y <- Function(1, 2, 3)  # Does something more complicated
#' y                       # Display result
HierarchicalKmeans <- function(X,
                               k=7,
                               ids=c(1:nrow(X)),
                               use.columns=c(1:ncol(X)),
                               omit.columns=setdiff(1:ncol(X), use.columns),
                               ...
                               ) {
  # Guardians
  stopifnot(class(X) == "data.frame" || class(X <- as.data.frame(X)) == "data.frame",
            all((k %% 1) == 0) && k >= 2,
            (is(use.columns, "integer") && all(use.columns %in% 1:ncol(X))) || 
              (is(use.columns, "logical") && length(use.columns) == ncol(X)),
            is(ids, "integer") && length(ids) == nrow(X),
            require(stats) )
  
  # deal with default and missing values
  k <- k[1]
  if( is(use.columns, "logical") ) use.columns <- which(use.columns)
  use.columns <- unique(use.columns)
  if( !missing(omit.columns) ) use.columns <- setdiff(1:ncol(X), omit.columns)
  
  n.distinct.rows <- nrow(X) - sum(duplicated(X[,use.columns]))
  k <- min(k, n.distinct.rows)
  
  # perform the function
  if(nrow(X) <= k || k == 1 ) {
    new.subclusters <- NULL
  } else {
    new.clusters <- kmeans(x=X[,use.columns], 
                           centers=k,
                           ...)
    new.subclusters <- lapply(1:k, function(i) {
      these.ids <- which(new.clusters$cluster==i)
      return( list(ids=these.ids,
                   subclusters=HierarchicalKmeans(X=X[these.ids,], 
                                                  k=k,
                                                  ids=these.ids,
                                                  use.columns=use.columns,
                                                  ...)) )
    })
  }
  
  # prepare and return the output
  return( list(ids=ids, subclusters=new.subclusters) )
}

# Test code for HierarchicalKmeans
# n.observations <- 100; n.attributes <- 10; k <- 7
# test.data <- matrix(rnorm(n.observations * n.attributes), nrow=n.observations)
# test.hkmeans <- HierarchicalKmeans(X=test.data, k=k)
# test.hkmeans
#####

ImmediateParent <- function(id, url, db) {
  # Test: ImmediateParent(url="http://www.chinavitae.com/institution/bus/8010.723011002", db=raw.china)
  # Test: ImmediateParent(url="http://www.chinavitae.com/institution/bus/8010.723011002", db=raw.china)
  # Test: ImmediateParent(url="http://www.chinavitae.com/institution/bus/8010.723011002", db=raw.china)
  # Test: ImmediateParent(url="http://www.chinavitae.com/institution/bus/8010.723011002", db=raw.china)
  parents <- ParentInstitutions(id, url, db)
  if( is.null(parents) ) return(NULL)
  return( as.character(parents[which.max(nchar(parents))]) )
}

is.institution <- function(url, db) return( url %in% db$institution$url )

LocationsWorkedByOfficial <- function(id, db, show.names=FALSE) {
  posting.ids <- db$postings$id[db$postings$official_id == id]
  
  location.ids <- numeric(0)
  for(this.posting.id in posting.ids) {
    location.ids <- c(location.ids, 
                      db$posting_locations$location_id[db$posting_locations$posting_id == this.posting.id])
  }
  location.ids <- sort(unique(location.ids))
  
  if( show.names ) {
    return( sapply)
  } else {
    return( location.ids )
  }
}

mgrep <- function(patterns, x, ignore.case = FALSE, perl = FALSE, 
                  value = FALSE, fixed = FALSE, useBytes = FALSE) {
  # Like grep, except that
  #  grep: one pattern and character vector to search
  #  mgrep: character vector of patterns and one character string to search within
  
  if(length(x) != 1) stop("x must be length 1, at least for now")
  
  good.rows <- sapply(patterns, function(this.pattern) {
    grepl(pattern=this.pattern, x=x, ignore.case=ignore.case, perl=perl,
          fixed=fixed, useBytes=useBytes)
  })
  if( value ) {
    return( patterns[good.rows] )
  } else {
    return( which(good.rows) )
  }
}

MoveCol <- function(df, from, after, before, first=FALSE, last=FALSE) {
  if( first ) {
    return( cbind(df[,from], df[,-from]) )
  } else if( last ) {
    return( cbind(df[,-from], df[,from]) )
  } else if( missing(after) & missing(before) ) {
    stop("Must specify destination for the column")
  } else if( missing(after) ) {
    if(1 == before | names(df)[1] == before) {
      return( MoveCol(df=df, from=from, first=TRUE) )
    } else {
      
    }
  } else {
    
  }
}

MySQLDatabaseToList <- function(dbname, channel) {
  # Given the name of a database and an 'RODBC' object resulting from odbcConnect
  # returns a list where each element is a dataframe containing a table from the database.
  if( sqlQuery(channel, paste("USE", dbname)) != "No Data" ) stop("Unknown database")
  table.list <- sqlQuery(channel, "SHOW TABLES")[,1]
  out <- sapply(table.list, function(tab) sqlQuery(channel, paste("SELECT * FROM", tab), stringsAsFactors=FALSE))
  names(out) <- table.list
  return( out )
}

OfficialPostingLocations <- function(id, db, return.dates=FALSE) {
  # Given the id of an official returns the id,location of locations where that official was posted.
  # If return.dates==FALSE (default) returns one row per official-location.
  # If return.dates==TRUE returns one row per official-posting with start_year and end_year.
  #
  # Test: OfficialPostingLocations(id=0, db=china)
  # Test: OfficialPostingLocations(id=0, db=china, return.dates=TRUE)
  if( return.dates ) {
    posting.locations <- db$posting_locations[db$posting_locations$official_id == id,]
    out <- merge(x=posting.locations, y=db$postings, by.x="posting_id", by.y="id", all.x=TRUE, all.y=FALSE)
    out$location <- sapply(out$location_id,
                           function(this.id) {
                             return( db$locations$location[db$locations$id == this.id] )
                           })
  } else {
    location.ids <- db$posting_locations$location_id[db$posting_locations$official_id == id]
    out <- db$locations[db$locations$id %in% location.ids,]
  }
  return( out )
}

ParentInstitutions <- function(id, url, db) {
  # Given id or url of an institution
  # returns vector of ids or urls of superset (url is a subset) institutions.
  #
  # Test: found.urls <- ParentInstitutions(url="http://www.chinavitae.com/institution/sta/3200.808001", db=raw.china)
  # Test: identical("http://www.chinavitae.com/institution/sta/3200.808", found.urls)
  # Test: ParentInstitutions(url="http://www.chinavitae.com/institution/sta/3200.808", db=raw.china) # example of missing parent "http://www.chinavitae.com/institution/sta/3200."
  if(missing(id)) {
    if( missing(url) ) stop("Must specify either id or url of the ego institution")
    rows.to.keep <- mgrep(patterns=paste(db$institutions$url, ".+", sep=""), 
                          x=url)
    out <- db$institutions$url[rows.to.keep]
  } else {
    url <- db$institutions$url[db$institutions$id==id]
    rows.to.keep <- mgrep(patterns=paste(db$institutions$url, ".+", sep=""), 
                          x=url)
    out <- db$institutions$id[rows.to.keep]
  }
  if( 0 == length(out) ) return( NULL )
  else return( as.character(out) )
}

RenameCol <- function(df, from, to=from) {
  # Test: this.df <- data.frame(a=1, b="clam"); this.df <- RenameCol(df=this.df, from="b", to="c"); this.df
  if(length(from) > 1) stop("It's not safe to rename more than one column at a time.")
  if( any(grepl(paste("^", to, "$", sep=""), names(df))) ) stop("There is already a column with that name.")
  if(sum(names(df)==from)==1) names(df)[names(df)==from] <- to
  return(df)
}
