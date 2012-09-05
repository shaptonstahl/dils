# Create workunit control file, workunit.RData

# Nomo:    setwd("/home/srh/svn/abstentions/batch")
# Munster: setwd("C:/svn/abstentions/batch")
# HapLap:  setwd("C:/svn/abstentions/batch")
# Day:     setwd("C:/svn/abstentions/batch")

source("http://sheer.ucdavis.edu/svn/software/public/unfactorColumns/unfactorColumns.R")

generate.workunit.control <- function(params) {
  # Generates a data.frame with all combinations of parameters so that 
  # the first parameter changes slowest, the last one changes fastest.
  out <- rev(expand.grid(rev(params)))
  names(out) <- names(params)
  return(out)
}

workunits <- generate.workunit.control(list(
  ab.rate=c(.3, .1, .05),
  mech=c("compete", "indiff"),
  legislature=1:200,
  legis.dir="generated_legislature"
))
workunits <- rbind(workunits, generate.workunit.control(list(
  ab.rate=0,
  mech="full",
  legislature=1:200,
  legis.dir=c("generated_legislature", "generated_legislature/sparse_abstentions")
)))
workunits <- rbind(workunits, generate.workunit.control(list(
  ab.rate=.05,
  mech=c("compete"),
  legislature=1:200,
  legis.dir="generated_legislature/sparse_abstentions"
)))

workunits <- unfactorColumns(workunits)

save(workunits, file="minion_nominate_workunits.RData")
# nrow(workunits)
