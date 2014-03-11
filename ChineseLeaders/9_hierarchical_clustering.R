# Provide for each node of the DILS or Dumb DILS network for officials
#
# DILS: Data-Informed Link Strength
# Dumb DILS
# FastDILS: approximate DILS by starting with 
#   
# Save as .rds files,
#         .tsv (tab-separated),
#         .txt (one column with corresponding ids)

# Remove detrius and start with a clean session
source("external_code/Decruft.R")

#####  Load libraries  #####
source("http://www.haptonstahl.org/R/usePackage/usePackage.R")  # provides UsePackage
UsePackage("rjson")           # provides access to MySQL database
UsePackage("cluster")           # provides access to MySQL database

#####  Set options  #####
# options(stringsAsFactors = FALSE)

#####  Load functions  #####
source("0_functions.R")

#####  Load data from earlier step  #####
official.hash <- readRDS(file="7_offical_hash.rds")
# all.nets.sample <- all.nets[sample(1:nrow(all.nets), 100),]
# DataFrameClasses(official.hash)
# head(official.hash)

# coerce from complex to real (Re)
official.hash <- ComplexToReal(official.hash)
# DataFrameClasses(official.hash)
# head(official.hash)
# nrow(official.hash)

# official.hash.sample.6 <- official.hash[sample(1:nrow(official.hash), 6),]
# system.time(dd.hkmeans.sample.6 <- HierarchicalKmeans(official.hash.sample.6, k=7, omit.columns=1:2))
# dd.hkmeans.sample.6
# 
# official.hash.sample.7 <- official.hash[sample(1:nrow(official.hash), 7),]
# system.time(dd.hkmeans.sample.7 <- HierarchicalKmeans(official.hash.sample.7, k=7, omit.columns=1:2))
# dd.hkmeans.sample.7
# 
# official.hash.sample.8 <- official.hash[sample(1:nrow(official.hash), 8),]
# system.time(dd.hkmeans.sample.8 <- HierarchicalKmeans(X=official.hash.sample.8, k=7, omit.columns=1:2))
# dd.hkmeans.sample.8
# 
# official.hash.sample.200 <- official.hash[sample(1:nrow(official.hash), 200),]
# system.time(dd.hkmeans.sample.200 <- HierarchicalKmeans(official.hash.sample.200, k=7, omit.columns=1:2))
# 
# many <- 3000
# official.hash.sample.many <- official.hash[sample(1:nrow(official.hash), many),]
# system.time(dd.hkmeans.sample.many <- HierarchicalKmeans(official.hash.sample.many, k=7, omit.columns=1:2, iter.max=50))

# Agglonerative hierarchical clustering
system.time(dd.agnes <- agnes(official.hash))

# Hierarchical k-Means
system.time(dd.hkmeans <- HierarchicalKmeans(official.hash, k=7, omit.columns=1:2, iter.max=50))

# save work
saveRDS(dd.agnes, file="9_aglomerative_hierarchical_clustering.rds")
write.table(dd.agnes$merge, 
            file="9_aglomerative_hierarchical_clustering.tsv",
            quote=FALSE,
            sep="\t",
            row.names=FALSE,
            col.names=FALSE)

saveRDS(dd.hkmeans, file="9_Hierarchical_kMeans_k7.rds")
cat(toJSON(dd.hkmeans), file="9_Hierarchical_kMeans_k7.json")