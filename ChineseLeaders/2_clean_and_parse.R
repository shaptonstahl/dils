# Clean and parse Chinese Leader data I imported from MySQL
# http://www.chinavitae.com/biography_browse.php
# 
# Do basic cleaning and parsing.
#  
# Save as .rds files (one per R object).

# Remove detrius and start with a clean session
source("external_code/Decruft.R")

#####  Load libraries  #####
source("http://www.haptonstahl.org/R/usePackage/usePackage.R")  # provides UsePackage
UsePackage("gdata")  # provides unknownToNA
UsePackage("XML")    # provides

#####  Set options  #####
# n.cpus <- 8
options(stringsAsFactors = FALSE)

#####  Load functions  #####
source("0_functions.R")

#####  Load data form last step in workflow  #####
raw.china <- readRDS("1_output_raw_china.rds")
RAW.CHINA.UNALTERED <- raw.china

#####  table: institutions  #####
# Remove column 'category'
raw.china$institutions <- raw.china$institutions[,-grep("category", names(raw.china$institutions))]

# add top-level institutions
links <- ExtractLinksAndTitles("http://www.chinavitae.com/institution")
top.level.links <- links[grepl("^/institution/.+", links$url),]
top.level.urls <- paste("http://www.chinavitae.com", top.level.links$url, sep="")
raw.china$institutions <- rbind(raw.china$institutions, 
                                data.frame(id=GetGUIDs(length(top.level.links)),
                                           name=top.level.links$title,
                                           url=top.level.urls))
# add id of parent institution
raw.china$all.institutions$parent.id <- sapply(raw.china$all.institutions$id, 
                                               function(ego.id) {
                                                 ip <- ImmediateParent(id=ego.id, db=raw.china)
                                                 if( is.null(ip) ) return(NA)
                                                 else return(ip)
                                               })

# Table: locations #
raw.china$locations <- unknownToNA(raw.china$locations, unknown=-1)
raw.china$locations <- unknownToNA(raw.china$locations, unknown="null")

# Table: officials #
raw.china$officials <- unknownToNA(raw.china$officials, unknown=-1)
raw.china$officials <- unknownToNA(raw.china$officials, unknown="null")

# Table: postings
raw.china$postings <- unknownToNA(raw.china$postings, unknown=-1)
raw.china$postings <- unknownToNA(raw.china$postings, unknown="null")


#####  cross-check ids  #####
# officials
AnyBadIds(raw.china$officials$nationality_id, raw.china$nationalities$id)
# posting_institutions
AnyBadIds(raw.china$posting_institutions$official_id, raw.china$officials$id)
AnyBadIds(raw.china$posting_institutions$institution_id, raw.china$institutions$id)
AnyBadIds(raw.china$posting_institutions$posting_id, raw.china$postings$id)
# posting_locations
AnyBadIds(raw.china$posting_locations$official_id, raw.china$officials$id)
AnyBadIds(raw.china$posting_locations$location_id, raw.china$locations$id)
AnyBadIds(raw.china$posting_locations$posting_id, raw.china$postings$id)
# postings
AnyBadIds(raw.china$postings$official_id, raw.china$officials$id)
AnyBadIds(raw.china$postings$title_id, raw.china$titles$id)

# Get rid of underscaores in cariable names
names(raw.china) <- gsub("_", ".", names(raw.china))
raw.china <- lapply(raw.china, function(this.table) {
  names(this.table) <- gsub("_", ".", names(this.table))
  return(this.table)
})

# Save cleaned data
saveRDS(raw.china, file="2_output_china_data_cleaned.rds")

