# Import Chinese Leader data that Kirill scraped from
# http://www.chinavitae.com/biography_browse.php
# 
# Do basic cleaning and parsing.
#  
# Save as .rds files (one per R object).

# Remove detrius and start with a clean session
source("external_code/Decruft.R")

#####  Load libraries  #####
source("http://www.haptonstahl.org/R/usePackage/usePackage.R")  # provides UsePackage
UsePackage("RODBC")           # provides access to MySQL database
UsePackage("rChoiceDialogs")  # provides rchoose.dir

#####  Set options  #####
#n.cpus <- 8
options(stringsAsFactors = FALSE)

# Load functions
source("0_functions.R")

#####  Read data from MySQL, clean, and store as RDS  #####
channel <- odbcConnect("srh-test-server")  # establish connection to locally hosted Ubuntu VM MySQL database
# remember to odbcClose(channel) later
# format for ad-hoc queries:  sqlQuery(channel, "SQL GOES HERE")
# sqlQuery(channel, "SHOW DATABASES")
# sqlQuery(channel, "SHOW TABLES")

raw.china <- MySQLDatabaseToList("chinavita", channel)
odbcClose(channel)  # Close the connection


saveRDS(raw.china, file="1_output_raw_china.rds")