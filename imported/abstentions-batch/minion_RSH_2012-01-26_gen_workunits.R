# Create workunit control file, workunit.RData

# Nomo:    setwd("/home/srh/svn/abstentions/batch")
# Munster: setwd("E:/svn/abstentions/batch")
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

one.legislature <- generate.workunit.control(list(
  ab.rate=0,
  mech="full",
  model="cjr",
  legis.dir="generated_legislature"
))
one.legislature <- rbind(one.legislature, generate.workunit.control(list(
  ab.rate=c(.05, .1, .3),
  mech=c("compete"),
  model=c("nominate", "cjr", "rsh"),
  legis.dir="generated_legislature"
)))
one.legislature

workunits <- data.frame(legislature=1, one.legislature)
for(i in 2:200)  workunits <- rbind(workunits, data.frame(legislature=i, one.legislature))

workunits <- unfactorColumns(workunits)

save(workunits, file="../batch/minion_RSH_2012-01-26_workunits.RData")
# load(file="minion_RSH_2012-01-26_workunits.RData")
# nrow(workunits)
