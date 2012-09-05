# minion_RSH_2011-08-26.R
# 
# Estimate NOMINATE, CJR, or RSH-compete on either all votes or
# observed votes for each of a number of simulated legislatures.

# The project-specific source file does three things.
# First, it checks to see if needed packages are installed.
# If not, it tries to install them.  If it cannot load the required 
# packages, it reports back failure to do_workunit.R.
# Second, if it can load the required packages, it completes the
# work unit assigned.  This likely involves downloading a
# project-specific task list with the parameters for the work
# unit, downloading data, etc.
# Third, it reports back to do_workunit.R, either with success and an
# R object specifying data to be saved or with failure.

# Uses 'project.workunit'. Sets 'minion.success' (TRUE/FALSE), 'minion.output' (list), minion.output.file.name (character)

setwd("~")

packages.loaded <- UsePackage("MCMCpack") && UsePackage("xtable") && UsePackage("wnominate") && UsePackage("runjags") && UsePackage("abind") && UsePackage("pscl")
source("http://sheer.ucdavis.edu/svn/software/public/BugsFunctions/BugsFunctions.R")

# temp fix for old runjags
# success <- TRUE

if(packages.loaded) {
  # Load workunit index:  'workunits'
  con <- url("http://sheer.ucdavis.edu/projects/abstentions/batch/minion_RSH_2012-01-09_workunits.RData")
  load(con); close(con)
  
  # Load data: this.legislature
  con <- url(paste("http://sheer.ucdavis.edu/projects/abstentions/generated_legislature/leg", 
    workunits$legislature[project.workunit], ".RData", sep=""))
  load(con); close(con)
  
  n.legislators <- this.legislature$gen.params$n.legislators
  n.item <- this.legislature$gen.params$n.bills

  if( "full" == workunits$mech[project.workunit] ) {
    # Run CJR on full data using MCMCpack
    rc <- this.legislature$rc.full
    constraints <- list(-2, 2)
    names(constraints) <- c("V1", paste("V", n.legislators, sep=""))
    result <- MCMCirt1d(rc,
      theta.constraints=constraints,
      verbose=10000,
      T0=.25,
      thin=50,
      burnin=20000,
      mcmc=50000)
    result.type <- "MCMCirt1d"
    minion.output.file.name <- paste("RSH_2012-01-09_CJR_on_all_votes_leg", 
      workunits$legislature[project.workunit], ".RData", sep="")
  } else {
    rc <- this.legislature[[workunits$mech[project.workunit]]][[as.character(workunits$ab.rate[project.workunit])]]$rc
    
      #############
      ###  CJR  ###
      #############
      constraints <- list(-2, 2)
      names(constraints) <- c("V1", paste("V", n.legislators, sep=""))
      result <- MCMCirt1d(rc,
        theta.constraints=constraints,
        verbose=10000,
        T0=.25,
        thin=50,
        burnin=20000,
        mcmc=50000)
      result.type <- "MCMCirt1d"
      minion.output.file.name <- paste("RSH_2012-01-09_CJR_on_", 
        workunits$mech[project.workunit], "_at_", 
        workunits$ab.rate[project.workunit], "_leg", 
        workunits$legislature[project.workunit], ".RData", sep="")
  }
  
  # collect results
  minion.output <- list(
    minion.workunit=minion.workunit, 
    url.project.batch=url.project.batch,
    project.workunit=project.workunit,
    legislature=workunits$legislature[project.workunit],
    mech=workunits$mech[project.workunit],
    model=workunits$model[project.workunit],
    ab.rate=workunits$ab.rate[project.workunit],
    result.type=result.type,
    result=result)
  minion.success <- TRUE
}
# else: report failure
