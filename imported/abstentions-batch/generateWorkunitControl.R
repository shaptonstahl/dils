# Create workunit control file, workunit.RData

# Nomo:    setwd("/home/srh/svn/abstentions/batch")
# Munster: setwd("C:/svn/abstentions/batch")
# HapLap:  setwd("C:/svn/abstentions/batch")
# Day:     setwd("C:/svn/abstentions/batch")

source("http://svn.wustl.edu/srhapton/software/software/public/unfactorColumns/unfactorColumns.R")

generate.workunit.control <- function(params) {
  # Generates a data.frame with all combinations of parameters so that 
  # the first parameter changes slowest, the last one changes fastest.
  out <- rev(expand.grid(rev(params)))
  names(out) <- names(params)
  return(out)
}

workunits <- generate.workunit.control(list(
#  ab.rate=c(.3, .1, .05),
  ab.rate=c(.1),
  legislature=1:200,
#  ab.rate=c(.05),
#  mech=c("indiff", "extreme", "compete", "competeNoMAR")
#  mech=c("indiff", "extreme", "compete")
#  mech=c("indiff", "extreme", "competeMaj")
#  mech=c("indiff", "alien")
  mech=c("compete")
))

# Remove completed units
op <- readLines("output.txt")
op <- substr(op, 11, 100)
op <- sort(as.numeric(sub("-.*", "", op)))
workunits <- workunits[!(workunits$legislature %in% op),]

# Add work units for CJR on data with no abstentions
#workunits <- rbind(workunits, data.frame(
#  ab.rate=0, 
#  legislature=1:200, 
#  mech="cjrComplete"))

workunits <- unfactorColumns(workunits)

save(workunits, file="workunits.RData")
nrow(workunits)
