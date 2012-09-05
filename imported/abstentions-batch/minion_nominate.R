# minion_project_nominate.R
# 
# Run NOMINATE twice on each of a series of simulated legislatures
# - Once on observed data (some rollcalls are missing)
# - Once on all data (all rollcalls available)


# The project-specific source file does three things.
# First, it checks to see if needed packages are installed.
# If not, it tries to install them.  If it cannot load the required 
# packages, it reports back failure to this script.
# Second, if it can load the required packages, it completes the
# work unit assigned.  This likely involves downloading a
# project-specific task list with the parameters for the work
# unit, downloading data, etc.
# Third, it reports back to this script, either with success and an
# R object specifying data to be saved or with failure.

# Uses 'project.workunit'. Sets 'minion.success' (TRUE/FALSE), 'minion.output' (list), minion.output.file.name (character)

packages.loaded <- UsePackage("wnominate")  # Use && to concatenate packages necessary for this project

if(packages.loaded) {
  # Load workunit index:  'workunits'
  con <- url("http://sheer.ucdavis.edu/projects/abstentions/batch/minion_nominate_workunits.RData")
  load(con); close(con)
  
  # Load data: this.legislature
  con <- url(paste("http://sheer.ucdavis.edu/projects/abstentions/", 
    workunits$legis.dir[project.workunit], "/leg", 
    workunits$legislature[project.workunit], ".RData", sep=""))
  load(con); close(con)
  
  if( "full" == workunits$mech[project.workunit] ) {
    rc <- rollcall(this.legislature$rc.full)
    minion.output.file.name <- paste("NOMINATE-full-leg", 
      workunits$legislature[project.workunit], 
      ifelse("generated_legislature" == workunits$legis.dir[project.workunit],
        "-dense", "-sparse"),
      ".RData", sep="")
  } else {
    rc <- rollcall(this.legislature[[workunits$mech[project.workunit]]][[as.character(workunits$ab.rate[project.workunit])]]$rc)
    minion.output.file.name <- paste("NOMINATE-", 
      workunits$mech[project.workunit], 
      "-leg", workunits$legislature[project.workunit], 
      "-rate", workunits$ab.rate[project.workunit], 
      ifelse("generated_legislature" == workunits$legis.dir[project.workunit],
        "-dense", "-sparse"),
      ".RData", sep="")
  }
  
  # run wnominate
  wnominate.output <- wnominate(rcObject=rc, dims=1, polarity=this.legislature$gen.params$n.legislators - 1)
  
  # collect results
  minion.output <- list(
    minion.workunit=minion.workunit, 
    url.project.batch=url.project.batch,
    project.workunit=project.workunit,
    legislature=workunits$legislature[project.workunit],
    legis.dir=workunits$legis.dir[project.workunit],
    mech=workunits$mech[project.workunit],
    ab.rate=workunits$ab.rate[project.workunit],
    wnominate.output=wnominate.output)
  minion.success <- TRUE
}
# else: report failure
