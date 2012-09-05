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
  mech=c("compete", "indiff"),
#  model=c("nominate", "cjr", "rsh"),
  model=c("cjr"),
  legis.dir="generated_legislature"
)))
one.legislature

workunits <- data.frame(legislature=1, one.legislature)
for(i in 2:400)  workunits <- rbind(workunits, data.frame(legislature=i, one.legislature))

# Remove NOMINATE runs for 30% missing data on small legislatures (1:200)
workunits <- workunits[!(workunits$legislature <= 200 & workunits$ab.rate==0.30 & workunits$model=="nominate"),]

workunits <- unfactorColumns(workunits)

save(workunits, file="../batch/minion_RSH_2011-08-26_workunits.RData")
# nrow(workunits)
