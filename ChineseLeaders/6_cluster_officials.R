# Cluster officials
#  
# Save as .rds files (one adjacency matrix),
#         .tsv (tab-separated square adjacency matrix),
#         .txt (one column with corresponding ids)

# Remove detrius and start with a clean session
source("external_code/Decruft.R")

#####  Load libraries  #####

#####  Set options  #####
options(stringsAsFactors = FALSE)
output.folder <- "clusters/hclust - complete/"

#####  Load functions  #####
source("0_functions.R")

#####  Load data form last step in workflow  #####
china <- CHINA.UNALTERED <- readRDS("2_output_china_data_cleaned.rds")
china$officials <- readRDS(file="3_ChinaLeaders_with_net_vars.rds")
n.officials <- nrow(china$officials)
n.institutions <- nrow(china$institutions)

# names(china$officials)

## Fill NAs with dummy values
# apply(china$officials, 2, function(x) sum(is.na(x)))  # columns with values to fill
china$officials$bio.chinese.name[is.na(china$officials$bio.chinese.name)] <- "unknown"
china$officials$birthyear[is.na(china$officials$birthyear)] <- 0
china$officials$deathyear[is.na(china$officials$deathyear)] <- 9999
china$officials$nationality.id[is.na(china$officials$nationality.id)] <- "unknown"
# sum(is.na(china$officials))  #  Should be zero

## remove columns that don't make sense to cluster with
china$officials <- china$officials[,!grepl("full.bio", names(china$officials))]

# clustering
fit.hclust <- hclust(dist(china$officials))

write.table(fit.hclust$merge, 
            file=paste(output.folder, "merge.tsv", sep=""),
            quote=FALSE,
            sep="\t",
            row.names=FALSE,
            col.names=FALSE)
write.table(fit.hclust$order,
            file=paste(output.folder, "order.tsv", sep=""),
            quote=FALSE,
            sep="\t",
            row.names=FALSE,
            col.names=FALSE)

plot(fit.hclust, 
     labels=china$officials$bio.name,
     axes=FALSE,
     main="Chinese Leaders, CLustered by Similarity")

groups <- cutree(fit.hclust, k=5)
rect.hclust(fit.hclust, k=5, border="red")

##### Now, what's inside that fit.hclust
names(fit.hclust)
