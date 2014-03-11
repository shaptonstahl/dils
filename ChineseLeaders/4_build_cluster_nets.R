# Build cluster networks, where a link exists exactly when the dyad 
# shares the same value of some nominal variable.
#  
# Save as .rds files (one adjacency matrix),
#         .tsv (tab-separated square adjacency matrix),
#         .txt (one column with corresponding ids)

# Remove detrius and start with a clean session
source("external_code/Decruft.R")

#####  Load libraries  #####

#####  Set options  #####
options(stringsAsFactors = FALSE)

#####  Load functions  #####
source("0_functions.R")

#####  Load data form last step in workflow  #####
china <- CHINA.UNALTERED <- readRDS("2_output_china_data_cleaned.rds")
n.officials <- nrow(china$officials)
n.institutions <- nrow(china$institutions)

#####  Links for same nationality  #####
network.nationality <- CreateClusterNetwork(node.table=china$officials, 
                                            cluster.on="nationality.id",
                                            names.field="id")
ExportAdjacencyMatrix(network.nationality, 
                      loc.stem="networks/chinese_nationality")
