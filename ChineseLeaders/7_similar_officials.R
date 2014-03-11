# For each official give the similarity of each other official
# using an affinity score in [0,1].
#  
# To do this quickly, each official is hashed into a 
# vector.  The L_1 norm (taxicab) of the difference is a distance,
# and 1-distance is the affinity score.

# Remove detrius and start with a clean session
source("external_code/Decruft.R")

#####  Load libraries  #####
source("http://www.haptonstahl.org/R/usePackage/usePackage.R")  # provides UsePackage

#####  Set options  #####
options(stringsAsFactors = FALSE)

#####  Load functions  #####
source("0_functions.R")

imputation.unit <-complex(real=.5, imaginary=sqrt(3)/2)  # distance of 1 from both zero and one

#####  Load data form last step in workflow  #####
china <- CHINA.UNALTERED <- readRDS("2_output_china_data_cleaned.rds")

n.officials <- nrow(china$officials)
n.institutions <- nrow(china$institutions)

# Create dataframe of [0,1]-valued hashes for each official
china$official.hash <- china$officials[,c("id", "bio.name")]

  # Keep a running count of the number of variables (factors) to be used
  n.factors <- 0
  
  china$official.hash$age <- china$officials$birthyear / 100
  n.factors <- n.factors + 1
  china$official.hash$age[is.na(china$official.hash$age)] <- mean(china$official.hash$age, na.rm=TRUE)
  
  for(i in 1:nrow(china$nationalities)) {
    china$official.hash <- data.frame(china$official.hash, newcol=complex(real=0, imaginary=0))
    names(china$official.hash)[ncol(china$official.hash)] <- paste("nationality.", 
                                                                   sum(grepl("nationality", names(china$official.hash)))+1, 
                                                                   sep="")
    china$official.hash[which(china$officials$nationality.id==china$nationalities$id[i]), ncol(china$official.hash)] <- 1
  }
  n.factors <- n.factors + 1
  #china$official.hash <- china$official.hash[,!apply(china$official.hash, 2, function(x) all(x==0))]  # remove cols with no 1s
  #is.na(china$official.hash[,grepl("nationality", names(china$official.hash))]) <- imputation.unit    # fill NAs with something 1 from zero and one
  n.nationality.cols <- sum(grepl("nationality", names(china$official.hash)))
  china$official.hash[,grepl("nationality", names(china$official.hash))] <- china$official.hash[,grepl("nationality", names(china$official.hash))] / n.nationality.cols
  
  official.title.list <- sapply(1:n.officials, function(i) {
    title.ids <- china$postings$title.id[china$postings$official.id == china$officials$id[i]]
    return( china$titles$title[china$titles$id %in% title.ids] )
  })
  official.institution.list <- sapply(1:n.officials, function(i) {
    institution.ids <- china$posting.institutions$institution.id[china$posting.institutions$official.id==china$officials$id[i]]
    return( china$institutions$name[china$institutions$id %in% institution.ids] )
  })
  
  china$official.hash$CPC <- as.numeric(sapply(1:n.officials, function(i) {
    return( any(grepl("\\<CPC\\>", official.institution.list[[i]], ignore.case=TRUE)) )
  }, USE.NAMES=FALSE))
  n.factors <- n.factors + 1
  
  regex.flag <- c("general", "admiral", "defense")
  china$official.hash$mil.flag.rank <- as.numeric(sapply(1:n.officials, function(i) {
    return( any(grepl(pattern=paste(regex.flag, collapse="|"), 
                      official.title.list[[i]], ignore.case=TRUE)) )
  }, USE.NAMES=FALSE))
  n.factors <- n.factors + 1
  
  for(i in 1:nrow(china$titles)) {
    china$official.hash <- data.frame(china$official.hash, newcol=0)
    names(china$official.hash)[ncol(china$official.hash)] <- paste("title.", i, sep="")
    rows.to.update <- sapply(1:nrow(china$official.hash), function(i.official) {
      any(china$titles$title[i] %in% official.title.list[[i.official]])
    })
    china$official.hash[rows.to.update, ncol(china$official.hash)] <- 1
  }
  n.factors <- n.factors + 1
  china$official.hash <- china$official.hash[,!apply(china$official.hash, 2, function(x) all(x==0))]  # remove cols with no 1s
  china$official.hash[is.na(china$official.hash)] <- imputation.unit          # fill NAs with something 1 from zero and one
  n.title.cols <- sum(grepl("title", names(china$official.hash)))
  china$official.hash[,grepl("title", names(china$official.hash))] <- china$official.hash[,grepl("title", names(china$official.hash))] / n.title.cols

  saveRDS(china$official.hash, file="7_offical_hash.rds")

# Generate affinities
hashes <- as.matrix(china$official.hash[,3:ncol(china$official.hash)])
n.col.hashes <- ncol(hashes)
affinities <- matrix(0, nrow=n.officials, ncol=n.officials)
rownames(affinities) <- colnames(affinities) <- china$officials$id
for( i in 1:n.officials ) {
  for( j in 1:i ) {
    new.value <- sum(abs(hashes[i,3:n.col.hashes] - hashes[j,3:n.col.hashes])) / n.factors
    affinities[i,j] <- affinities[j,i] <- new.value
  }
  cat(i, ", ", j, " <- ", round(new.value, 2), "\n", sep="")
}

# Checks
# sum(affinities > 1)   # should be zero
# sum(affinities < 0)   # should be zero
# hist(affinities, 100)
# str(affinities)
     

# Save affinities in tab-separated value files
write.table(affinities,
            file="7_officials_affinities.tsv",
            quote=FALSE,
            sep="\t",
            row.names=FALSE,
            col.names=FALSE)
res <- zip("7_officials_affinities.zip", "7_officials_affinities.tsv")
if(0==res) file.remove("7_officials_affinities.tsv") else cat("Error zipping; try again")

write.table(head(affinities),
            file="7_officials_affinities_head.tsv",
            quote=FALSE,
            sep="\t",
            row.names=FALSE,
            col.names=FALSE)
write.table(colnames(affinities), 
            file="7_officials_affinities_ids.txt",
            quote=FALSE,
            sep="\t",
            row.names=FALSE,
            col.names=FALSE)

# Recast as quantiles
affinity.quantiles <- apply(affinities, c(1,2), ecdf(affinities))
# str(affinity.quantiles)
# head(affinity.quantiles)
# hist(affinity.quantiles)

write.table(affinity.quantiles,
            file="7_officials_affinity_quantiles.tsv",
            quote=FALSE,
            sep="\t",
            row.names=FALSE,
            col.names=FALSE)
res <- zip("7_officials_affinity_quantiles.zip", "7_officials_affinity_quantiles.tsv")
if(0==res) file.remove("7_officials_affinity_quantiles.tsv") else cat("Error zipping; try again")

write.table(head(affinity.quantiles),
            file="7_officials_affinity_quantiles_head.tsv",
            quote=FALSE,
            sep="\t",
            row.names=FALSE,
            col.names=FALSE)
