# Build age link adjacency matrix, a little smarter edition
#  
# Save as .rds files (one adjacency matrix),
#         .tsv (tab-separated square adjacency matrix),
#         .txt (one column with corresponding ids)

# Remove detrius and start with a clean session
source("external_code/Decruft.R")

#####  Set options  #####
options(stringsAsFactors = FALSE)

#####  Load functions  #####
source("0_functions.R")

#####  Load data form last step in workflow  #####
china <- CHINA.UNALTERED <- readRDS("2_output_china_data_cleaned.rds")
n.officials <- nrow(china$officials)
n.institutions <- nrow(china$institutions)

adjacency.matrix <- matrix(0, nrow=n.officials, ncol=n.officials)

adjacency.matrix <- sapply(china$officials$birthyear, function(this.yr) {
  return( sapply(china$officials$birthyear, function(that.yr) {
    if( is.na(this.yr) || is.na(that.yr) ) return(0)
    else return( as.numeric(abs(this.yr - that.yr) <= 5) )
  }) )
})
rownames(adjacency.matrix) <- colnames(adjacency.matrix) <- china$officials$id

ExportAdjacencyMatrix(adjacency.matrix, loc.stem="networks/chinese_age")
