# Create workunit control file, workunit.RData

# Nomo:    setwd("/home/srh/svn/abstentions/batch")
# Munster: setwd("C:/svn/abstentions/batch")
# HapLap:  setwd("C:/svn/abstentions/batch")
# Day:     setwd("C:/svn/abstentions/batch")

source("http://sheer.ucdavis.edu/svn/software/public/unfactorColumns/unfactorColumns.R")
source("http://sheer.ucdavis.edu/svn/software/public/batch/batch_functions.R")

workunits <- GenerateWorkunitControl(list(
  mech=c("full", "compete"),
  votes=c("all", "observed"),
  legislature=1:200
))

workunits <- unfactorColumns(workunits)

save(workunits, file="minion_abs_sparse_inits_workunits.RData")
# nrow(workunits)
