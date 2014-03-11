# Build binary networks

# Do basic cleaning and parsing.
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


#####  Links for being female  #####
network.female <- CreateDummyNetwork(node.table=china$officials, 
                                     dummy.name="sex",
                                     dummy.linked.value="female",
                                     names.field="id")
ExportAdjacencyMatrix(network.female, 
                      loc.stem="networks/chinese_female")
res <- zip("networks/chinese_female.zip", "networks/chinese_female.tsv")
if(0==res) file.remove("networks/chinese_female.tsv") else cat("Error zipping; try again")


#####  Links based on various posting titles or institutions (not time-aware)  #####
# The following is used for several variables:
#   has.been.mayor
#   has.been.educator
#   etc.
official.title.list <- sapply(1:n.officials, function(i) {
  title.ids <- china$postings$title.id[china$postings$official.id == china$officials$id[i]]
  return( china$titles$title[china$titles$id %in% title.ids] )
})
official.institution.list <- sapply(1:n.officials, function(i) {
  institution.ids <- china$posting.institutions$institution.id[china$posting.institutions$official.id==china$officials$id[i]]
  return( china$institutions$name[china$institutions$id %in% institution.ids] )
})

#####  Links for both having been a mayor  #####
china$officials$has.been.mayor <- as.numeric(sapply(1:n.officials, function(i) {
  return( any(grepl("mayor", official.title.list[[i]], ignore.case=TRUE)) )
}, USE.NAMES=FALSE))
network.mayor <- CreateDummyNetwork(node.table=china$officials, 
                                     dummy.name="has.been.mayor",
                                     dummy.linked.value=1,
                                     names.field="id")
ExportAdjacencyMatrix(network.mayor, 
                      loc.stem="networks/chinese_mayor")
res <- zip("networks/chinese_mayor.zip", "networks/chinese_mayor.tsv")
if(0==res) file.remove("networks/chinese_mayor.tsv") else cat("Error zipping; try again")

#####  Links for both having been an educator  #####
regex.educator <- c("professor", "dean", "teach", "academic", 
                    "lecture", "research", "instruct", "educ", 
                    "academy", "headmaster", "postdoc", "postgrad", 
                    "scholar", "principal", "principle")
china$officials$has.been.educator <- as.numeric(sapply(1:n.officials, function(i) {
  return( any(grepl(pattern=paste(regex.educator, collapse="|"), 
                    official.title.list[[i]], ignore.case=TRUE)) )
}, USE.NAMES=FALSE))
network.educator <- CreateDummyNetwork(node.table=china$officials, 
                                       dummy.name="has.been.educator",
                                       dummy.linked.value=1,
                                       names.field="id")
ExportAdjacencyMatrix(network.educator, 
                      loc.stem="networks/chinese_educator")
res <- zip("networks/chinese_educator.zip", "networks/chinese_educator.tsv")
if(0==res) file.remove("networks/chinese_educator.tsv") else cat("Error zipping; try again")

#####  Links for both having joined CPC  #####
china$officials$in.CPC <- as.numeric(sapply(1:n.officials, function(i) {
  return( any(grepl("\\<CPC\\>", official.institution.list[[i]], ignore.case=TRUE)) )
}, USE.NAMES=FALSE))
network.in.CPC <- CreateDummyNetwork(node.table=china$officials, 
                                     dummy.name="in.CPC",
                                     dummy.linked.value=1,
                                     names.field="id")
ExportAdjacencyMatrix(network.in.CPC, 
                      loc.stem="networks/chinese_CPC")
res <- zip("networks/chinese_CPC.zip", "networks/chinese_CPC.tsv")
if(0==res) file.remove("networks/chinese_CPC.tsv") else cat("Error zipping; try again")

#####  Links for both having joined CPPCC  #####
china$officials$in.CPPCC <- as.numeric(sapply(1:n.officials, function(i) {
  return( any(grepl("\\<CPPCC\\>", official.institution.list[[i]], ignore.case=TRUE)) )
}, USE.NAMES=FALSE))
network.in.CPPCC <- CreateDummyNetwork(node.table=china$officials, 
                                       dummy.name="in.CPPCC",
                                       dummy.linked.value=1,
                                       names.field="id")
ExportAdjacencyMatrix(network.in.CPPCC, 
                      loc.stem="networks/chinese_CPPCC")
res <- zip("networks/chinese_CPPCC.zip", "networks/chinese_CPPCC.tsv")
if(0==res) file.remove("networks/chinese_CPPCC.tsv") else cat("Error zipping; try again")

#####  Links for both having joined NPC  #####
china$officials$in.NPC <- as.numeric(sapply(1:n.officials, function(i) {
  return( any(grepl("\\<NPC\\>", official.institution.list[[i]], ignore.case=TRUE)) )
}, USE.NAMES=FALSE))
network.in.NPC <- CreateDummyNetwork(node.table=china$officials, 
                                     dummy.name="in.NPC",
                                     dummy.linked.value=1,
                                     names.field="id")
ExportAdjacencyMatrix(network.in.NPC, 
                      loc.stem="networks/chinese_NPC")
res <- zip("networks/chinese_NPC.zip", "networks/chinese_NPC.tsv")
if(0==res) file.remove("networks/chinese_NPC.tsv") else cat("Error zipping; try again")

#####  Links for both having been an engineer  #####
regex.engineer <- c("engineer", "architect", "water", "fire", 
                    "plant", "fluid", "aerospace", "mechanist", 
                    "technical", "technologist")
china$officials$has.been.engineer <- as.numeric(sapply(1:n.officials, function(i) {
  return( any(grepl(pattern=paste(regex.engineer, collapse="|"), 
                    official.title.list[[i]], ignore.case=TRUE)) )
}, USE.NAMES=FALSE))
network.engineer <- CreateDummyNetwork(node.table=china$officials, 
                                       dummy.name="has.been.engineer",
                                       dummy.linked.value=1,
                                       names.field="id")
ExportAdjacencyMatrix(network.engineer, 
                      loc.stem="networks/chinese_engineer")
res <- zip("networks/chinese_engineer.zip", "networks/chinese_engineer.tsv")
if(0==res) file.remove("networks/chinese_engineer.tsv") else cat("Error zipping; try again")

#####  Links for both having been an scientist  #####
regex.scientist <- c("scientist", "science", "scientific", "agronomist", 
                     "research", "economist", "economic", "geologist", 
                     "geologic", "academy", "physicist", "R&D", 
                     "postdoc", "postgrad")
china$officials$has.been.scientist <- as.numeric(sapply(1:n.officials, function(i) {
  return( any(grepl(pattern=paste(regex.scientist, collapse="|"), 
                    official.title.list[[i]], ignore.case=TRUE)) )
}, USE.NAMES=FALSE))
network.scientist <- CreateDummyNetwork(node.table=china$officials, 
                                       dummy.name="has.been.scientist",
                                       dummy.linked.value=1,
                                       names.field="id")
ExportAdjacencyMatrix(network.scientist, 
                      loc.stem="networks/chinese_scientist")
res <- zip("networks/chinese_scientist.zip", "networks/chinese_scientist.tsv")
if(0==res) file.remove("networks/chinese_scientist.tsv") else cat("Error zipping; try again")

#####  Links for both having been an commissar  #####
regex.commissar <- c("com{1,2}is{1,2}[ae]r", "politic")
china$officials$has.been.commissar <- as.numeric(sapply(1:n.officials, function(i) {
  return( any(grepl(pattern=paste(regex.commissar, collapse="|"), 
                    official.title.list[[i]], ignore.case=TRUE)) )
}, USE.NAMES=FALSE))
network.commissar <- CreateDummyNetwork(node.table=china$officials, 
                                        dummy.name="has.been.commissar",
                                        dummy.linked.value=1,
                                        names.field="id")
ExportAdjacencyMatrix(network.commissar, 
                      loc.stem="networks/chinese_commissar")
res <- zip("networks/chinese_commissar.zip", "networks/chinese_commissar.tsv")
if(0==res) file.remove("networks/chinese_commissar.tsv") else cat("Error zipping; try again")

#####  Links for both having been in law  #####
regex.in.law <- c("judge", "\\<judic", "magistrate", "lawyer", 
                  "\\<laws?\\>", "legal", "bailiff", "attorney", 
                  "councilor", "counsel")
china$officials$has.been.in.law <- as.numeric(sapply(1:n.officials, function(i) {
  return( any(grepl(pattern=paste(regex.in.law, collapse="|"), 
                    official.title.list[[i]], ignore.case=TRUE)) )
}, USE.NAMES=FALSE))
network.in.law <- CreateDummyNetwork(node.table=china$officials, 
                                        dummy.name="has.been.in.law",
                                        dummy.linked.value=1,
                                        names.field="id")
ExportAdjacencyMatrix(network.in.law, 
                      loc.stem="networks/chinese_law")
res <- zip("networks/chinese_law.zip", "networks/chinese_law.tsv")
if(0==res) file.remove("networks/chinese_law.tsv") else cat("Error zipping; try again")

#####  Links for both having been flag officer  #####
regex.flag <- c("general", "admiral", "defense")
china$officials$has.been.flag <- as.numeric(sapply(1:n.officials, function(i) {
  return( any(grepl(pattern=paste(regex.flag, collapse="|"), 
                    official.title.list[[i]], ignore.case=TRUE)) )
}, USE.NAMES=FALSE))
network.flag <- CreateDummyNetwork(node.table=china$officials, 
                                     dummy.name="has.been.flag",
                                     dummy.linked.value=1,
                                     names.field="id")
ExportAdjacencyMatrix(network.flag, 
                      loc.stem="networks/chinese_flag")
res <- zip("networks/chinese_flag.zip", "networks/chinese_flag.tsv")
if(0==res) file.remove("networks/chinese_flag.tsv") else cat("Error zipping; try again")

#####  Links for both having been health worker  #####
regex.health <- c("medic", "physician", "surgeon", "surgery", "health")
china$officials$has.been.health <- as.numeric(sapply(1:n.officials, function(i) {
  return( any(grepl(pattern=paste(regex.health, collapse="|"), 
                    official.title.list[[i]], ignore.case=TRUE)) )
}, USE.NAMES=FALSE))
network.health <- CreateDummyNetwork(node.table=china$officials, 
                                   dummy.name="has.been.health",
                                   dummy.linked.value=1,
                                   names.field="id")
ExportAdjacencyMatrix(network.health, 
                      loc.stem="networks/chinese_health")
res <- zip("networks/chinese_health.zip", "networks/chinese_health.tsv")
if(0==res) file.remove("networks/chinese_health.tsv") else cat("Error zipping; try again")

#####  Links for both having been criminal  #####
regex.criminal <- c("prison", "parole", "sentence", "\\<persecut")
china$officials$has.been.criminal <- as.numeric(sapply(1:n.officials, function(i) {
  return( any(grepl(pattern=paste(regex.criminal, collapse="|"), 
                    official.title.list[[i]], ignore.case=TRUE)) )
}, USE.NAMES=FALSE))
network.criminal <- CreateDummyNetwork(node.table=china$officials, 
                                     dummy.name="has.been.criminal",
                                     dummy.linked.value=1,
                                     names.field="id")
ExportAdjacencyMatrix(network.criminal, 
                      loc.stem="networks/chinese_criminal")
res <- zip("networks/chinese_criminal.zip", "networks/chinese_criminal.tsv")
if(0==res) file.remove("networks/chinese_criminal.tsv") else cat("Error zipping; try again")

#####  Links for both having been diplomat  #####
regex.diplomat <- c("diplomat", "representative", "ambassador", 
                    "envoy", "attache")
china$officials$has.been.diplomat <- as.numeric(sapply(1:n.officials, function(i) {
  return( any(grepl(pattern=paste(regex.diplomat, collapse="|"), 
                    official.title.list[[i]], ignore.case=TRUE)) )
}, USE.NAMES=FALSE))
network.diplomat <- CreateDummyNetwork(node.table=china$officials, 
                                       dummy.name="has.been.diplomat",
                                       dummy.linked.value=1,
                                       names.field="id")
ExportAdjacencyMatrix(network.diplomat, 
                      loc.stem="networks/chinese_diplomat")
res <- zip("networks/chinese_diplomat.zip", "networks/chinese_diplomat.tsv")
if(0==res) file.remove("networks/chinese_diplomat.tsv") else cat("Error zipping; try again")

#####  Links for both having been representative  #####
#
# This could use more attention. Perhaps CPC terms or related
# could apply here.
#
regex.representative <- c("representative", "delegate", "trustee")
china$officials$has.been.representative <- as.numeric(sapply(1:n.officials, function(i) {
  return( any(grepl(pattern=paste(regex.representative, collapse="|"), 
                    official.title.list[[i]], ignore.case=TRUE)) )
}, USE.NAMES=FALSE))
network.representative <- CreateDummyNetwork(node.table=china$officials, 
                                       dummy.name="has.been.representative",
                                       dummy.linked.value=1,
                                       names.field="id")
ExportAdjacencyMatrix(network.representative, 
                      loc.stem="networks/chinese_representative")
res <- zip("networks/chinese_representative.zip", "networks/chinese_representative.tsv")
if(0==res) file.remove("networks/chinese_representative.tsv") else cat("Error zipping; try again")

#####  Links for both having been executive  #####
regex.executive <- c("executive", "minister", "secretary", 
                     "president", "chief", "head", "governor", 
                     "director", "chairman", "ceo")
china$officials$has.been.executive <- as.numeric(sapply(1:n.officials, function(i) {
  return( any(grepl(pattern=paste(regex.executive, collapse="|"), 
                    official.title.list[[i]], ignore.case=TRUE)) )
}, USE.NAMES=FALSE))
network.executive <- CreateDummyNetwork(node.table=china$officials, 
                                             dummy.name="has.been.executive",
                                             dummy.linked.value=1,
                                             names.field="id")
ExportAdjacencyMatrix(network.executive, 
                      loc.stem="networks/chinese_executive")
res <- zip("networks/chinese_executive.zip", "networks/chinese_executive.tsv")
if(0==res) file.remove("networks/chinese_executive.tsv") else cat("Error zipping; try again")

##### Save augmented china
saveRDS(china$officials, file="3_ChinaLeaders_with_net_vars.rds")