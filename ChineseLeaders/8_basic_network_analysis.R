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
UsePackage("igraph")           # provides access to MySQL database

#####  Set options  #####
options(stringsAsFactors = FALSE)

#####  Load functions  #####
# source("0_functions.R")

#####  Load each generated network and merge into an array  #####
dumb.dills <- readRDS("6_dumb_dills.rds")

dd.graph <- graph.adjacency(dumb.dills,
                            mode="undirected",
                            weighted=TRUE,
                            diag=FALSE)
# saveRDS(dd.graph, file="sna/dd_graph.rds")
# dd.graph <- readRDS(file="sna/dd_graph.rds")

#####  Generate network measures of "importance"
# For each official (node) four different measures of importance
# are generated.  Note:  Each of these takes 15-30 minutes to run.

dd.eigenvector.centrality <- evcent(dd.graph)
names(dd.eigenvector.centrality)
hist(dd.eigenvector.centrality$vector, 40)
# saveRDS(dd.eigenvector.centrality, file="sna/dd_eigenvector_centrality.rds")
# dd.eigenvector.centrality <- readRDS(file="sna/dd_eigenvector_centrality.rds")

dd.Bonacich.centralities <- bonpow(dd.graph)
names(dd.Bonacich.centralities)
hist(dd.Bonacich.centralities, 40)
# saveRDS(dd.Bonacich.centralities, file="sna/dd_Bonacich_centrality.rds")
# dd.Bonacich.centralities <- readRDS(file="sna/dd_Bonacich_centrality.rds")

dd.betweenness <- betweenness.estimate(dd.graph, cutoff=20)
names(dd.betweenness)
hist(dd.betweenness, 40)
# saveRDS(dd.betweenness, file="sna/dd_betweenness.rds")
# dd.betweenness <- readRDS(file="sna/dd_betweenness.rds")

system.time(dd.closeness <- closeness(dd.graph)
names(dd.closeness)
hist(dd.closeness, 40)
# saveRDS(dd.closeness, file="sna/dd_closeness.rds")
# dd.closeness <- readRDS(file="sna/dd_closeness.rds")
            

#####  Load data form last step in workflow  #####
china <- readRDS("2_output_china_data_cleaned.rds")
n.officials <- nrow(china$officials)
n.institutions <- nrow(china$institutions)

china$officials$net.centrality <- dd.Bonacich.centralities
china$officials$net.betweenness <- dd.betweenness
china$officials$net.eigenvector.centrality <- dd.eigenvector.centrality$vector
china$officials$net.closeness <- dd.closeness

saveRDS(china, file="Chineseleaders.rds")
write.table(china$officials, file="officials.tsv", row.names=FALSE, sep="\t", quote=FALSE)