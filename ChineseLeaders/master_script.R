# batch to run all R code for analyzing Chinese leaders

# Set the working directory to github:Berico-Technologies/network-analytics/ChineseLeaders
# setwd("C:/Users/shaptonstahl/Documents/GitHub/Berico-Technologies/network-analytics/ChineseLeaders")

# Run the code in this file one line at a time.
# Sourcing the entire file causes R to crash for reasons unknown.

source("1_import_from_MySQL.R")
source("2_clean_and_parse.R")
source("3_build_binary_nets.R")
source("4_build_cluster_nets.R")
source("5_build_age_links_smarter.R")

# Not yet implemented
# source("6_cluster_officials.R")
# source("external_code/Decruft.R")

source("7_similar_officials.R")

source("external_code/Decruft.R")