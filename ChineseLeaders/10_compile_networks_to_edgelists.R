# Convert adjacency matrices to edgelists

#' Confirm that all adjacency matrices have the same nodes
#' in the same order

library(stringr)  # provides str_locate_all and str_sub

source.data.folder <- "ChineseLeaders/networks/"

list.of.names.files <- dir(source.data.folder,
                           pattern="names")

comparison.list <- read.table(paste(source.data.folder, 
                                    list.of.names.files[1], sep=""),
                              stringsAsFactors=FALSE)

comparison.results <- sapply(list.of.names.files[2:length(list.of.names.files)],
                             function(this.file.name) {
                               this.list <- read.table(paste(source.data.folder, 
                                                             this.file.name, sep=""),
                                                       stringsAsFactors=FALSE)
                               return( identical(this.list, comparison.list) )
                             })
all(comparison.results)
# Yes, all adjacency matrices have the same nodes in the same order.

# Initialize chinese.edgelist with guids of from and to nodes
leader.guids <- comparison.list[,1]
n <- length(leader.guids)
chinese.edgelist <- expand.grid(leader.guids, leader.guids)
names(chinese.edgelist) <- c("from", "to")

list.of.networks <- sapply(list.of.names.files, function(this.file.name) {
  name.start <- str_locate_all(this.file.name, "_")[[1]][1,1] + 1
  name.end <- str_locate_all(this.file.name, "_")[[1]][2,1] - 1
  return( str_sub(this.file.name, name.start, name.end) )
})
names(list.of.networks) <- NULL

# Expand chinese.edgelist to include all variables (empty)
chinese.edgelist.values <- matrix(0, 
                                  nrow=nrow(chinese.edgelist),
                                  ncol=length(list.of.names.files))
colnames(chinese.edgelist.values) <- list.of.networks
head(chinese.edgelist.values)

list.of.adjacency.files <- dir(source.data.folder,
                               pattern="adjacency.tsv")
for( net.i in 1:length(list.of.adjacency.files) ) {
  cat("***  Processing", list.of.adjacency.files[net.i], " ***\n")
  
  cat("Reading adjacency matrix file\n")
  adj.matrix <- as.matrix(read.delim(paste(source.data.folder, 
                                           list.of.adjacency.files[net.i], sep=""),
                                     header=FALSE,
                                     stringsAsFactors=FALSE)
  )
  
  cat("Copying columns\n")
  pb <- txtProgressBar(max=n, style=2)
  for(to.j in 1:n) {
    chinese.edgelist.values[c(1:n) + (to.j - 1) * n, net.i] <- adj.matrix[,to.j]
    setTxtProgressBar(pb, to.j)
  }
  close(pb)
}

chinese.edgelist <- data.frame(chinese.edgelist, chinese.edgelist.values)
names(chinese.edgelist) <- c("from", "to", list.of.networks)
head(chinese.edgelist)

saveRDS(chinese.edgelist, 
        file=paste(source.data.folder, "chinese_all_networks_edgelist.rds", sep=""))
write.table(chinese.edgelist,
            file=paste(source.data.folder, "chinese_all_networks_edgelist.tsv", sep=""),
            quote=FALSE,
            sep="\t",
            row.names=FALSE,
            col.names=TRUE)