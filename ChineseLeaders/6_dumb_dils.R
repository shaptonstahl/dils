# Combine all the existing networks into a single network by taking 
# the dyadic mean, dropping missing values by dyad.
#  
# Save as .rds files (one adjacency matrix),
#         .tsv (tab-separated square adjacency matrix),
#         .txt (one column with corresponding ids)

# Remove detrius and start with a clean session
source("external_code/Decruft.R")

#####  Load libraries  #####
# source("http://www.haptonstahl.org/R/usePackage/usePackage.R")  # provides UsePackage
# UsePackage("abind")           # provides access to MySQL database

#####  Set options  #####
options(stringsAsFactors = FALSE)

#####  Load functions  #####
# source("0_functions.R")

#####  Verify that all networks have the nodes in the same order
network.name.files <- paste("networks/", dir("networks/", "network_names.txt"), sep="")
comparison.names <- read.table(network.name.files[1])
for( i in 1:length(network.name.files) ) {
  new.names <- read.table(network.name.files[i])
  stopifnot( identical(comparison.names, new.names) )
}
rm(network.name.files, i, new.names)
cat("All networks have identical node orders.\n")

#####  Load each generated network and merge into an array  #####
network.data.files <- dir("networks/", "network_adjacency.rds")
for( i in 1:length(network.data.files) ) {
  cat("Reading file ", i, "...", sep="")
  new.table <- readRDS(paste("networks/", network.data.files[i], sep=""))
  if( !exists("all.nets") ) {
    all.nets <- array(-1, dim=c(nrow(new.table), ncol(new.table), length(network.data.files)))
  }
  all.nets[,,i] <- new.table
  cat("complete.\n")
}
# dim(all.nets)
rm(network.data.files, new.table, i)

system.time( dumb.dills <- apply(all.nets, c(1,2), mean, na.rm=TRUE) )
# hist(dumb.dills)
colnames(dumb.dills) <- comparison.names[,1]
rownames(dumb.dills) <- comparison.names[,1]

saveRDS(all.nets, file="6_all_nets.rds")
saveRDS(dumb.dills, file="6_dumb_dills.rds")
write.table(dumb.dills,
            file="6_dumb_dills.tsv",
            quote=FALSE,
            sep="\t",
            row.names=FALSE,
            col.names=FALSE)
res <- zip("6_dumb_dills.zip", "6_dumb_dills.tsv")
if(0==res) file.remove("6_dumb_dills.tsv") else cat("Error zipping; try again")

write.table(comparison.names,
            file="6_dumb_dills_names.txt",
            quote=FALSE,
            sep="\t",
            row.names=FALSE,
            col.names=FALSE)
write.table(dumb.dills[100,100],
            file="6_dumb_dills_only_100.tsv",
            quote=FALSE,
            sep="\t",
            row.names=FALSE,
            col.names=FALSE)
write.table(comparison.names[1:100,],
            file="6_dumb_dills_only_100_names.txt",
            quote=FALSE,
            sep="\t",
            row.names=FALSE,
            col.names=FALSE)
