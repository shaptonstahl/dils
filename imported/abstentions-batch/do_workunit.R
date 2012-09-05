# do_workunit.R:  Main wrapper for batch work
# by Stephen R. Haptonstahl [srh@ucdavis.edu]
# 
# When an Internet-connected computer with R sources this file,
# it will ask the control server if there is any work to be done.
# 
# If so, the control server will pick a work unit and tell the client
# which project-specific source file and which work unit to complete.
# If not, the client will wait for a period of time, then try again.
# If there is no work for some period, the client will stop.
# 
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
# 
# This script takes the success or failure message.  If the work unit 
# failed, this script reports back to the control server that the work 
# unit failed.  This permits the work unit to be returned to the queue.
# If the work unit succeeded, this script uploads the results in two 
# forms: data to be stored in the MySQL database on the control server,
# and (optionally) an RData file to be stored on the server.

source("http://sheer.ucdavis.edu/svn/software/public/deCruft/deCruft.R")

##########################################################################
###  Control Server-Specific Parameters: These you may want to modify  ###
##########################################################################
# This file:  http://sheer.ucdavis.edu/projects/abstentions/batch/do_workunit.R
# Call with:  source("http://tinyurl.com/23zfotx")
base.url.batch.file <- "http://sheer.ucdavis.edu/projects/abstentions/"
file.name.batch.file <- "batch/batch.R"        # Name of this file
file.name.checkout <- "batch/checkout.php"
file.name.save.results.to <- "batch/save.php"
get.save.workunit.pw <- "ipotqqwwdcfokfnrllsvowifkhhtlthq"
welcome.message <- "Thank you for volunteering your computer to help with our research.  You can quit at any time by exiting R.  If you have any questions about this research, please contact Steve Haptonstahl [srh@ucdavis.edu], Guillermo Rosas [grosas@wustl.edu], or Yael Shomer [yshomer@gmail.com]."
goodbye.message <- "Thank you again for the use of your computer. If you have any questions about this research, please contact Steve Haptonstahl [srh@ucdavis.edu], Guillermo Rosas [grosas@wustl.edu], or Yael Shomer [yshomer@gmail.com]."
wait.before.trying.again.if.no.work.seconds <- 60 * 5
n.retries.if.no.work <- 12 * 24 * 7

###############################################################
###  Initialize: You should not have to modify these lines  ###
###############################################################
source("http://sheer.ucdavis.edu/svn/software/public/batch/batch_functions.R")

Say("Minion v1.0 by Stephen R. Haptonstahl [srh@ucdavis.edu]")
Announce(welcome.message)
host.name <- readline("What is the name of this computer?  ")

url.batch.file <- paste(base.url.batch.file, file.name.batch.file, sep="")
url.checkout <- paste(base.url.batch.file, file.name.checkout, sep="")
url.save.results.to <- paste(base.url.batch.file, 
  file.name.save.results.to, sep="")
n.retries <- 0
minion.output.mysql <- NULL
minion.output <- NULL
minion.success <- FALSE

###################
###  Main Loop  ###
###################
while(n.retries < n.retries.if.no.work) {
  # Ask for work unit
  workunit.response <- MinionGetWorkunit(get.from.url=url.checkout, 
    pw=get.save.workunit.pw, client=host.name)
  
  if("Hello, world!" == workunit.response) {
    Say(paste("No work available.  Minion will ask again in", 
      wait.before.trying.again.if.no.work.seconds, "seconds."))
    Sys.sleep(wait.before.trying.again.if.no.work.seconds)
    n.retries <- n.retries + 1
  } else {
    SourceCharacter(workunit.response)  # This creates 'project.workunit', 'url.project.batch', 'minion.workunit'
    
    source(url.project.batch)  # Uses 'project.workunit'. Sets 'minion.success' (TRUE/FALSE), 'minion.output' (list), minion.output.file.name (character)
    
    if(minion.success) {
      MinionUpload(url.save.results.to=url.save.results.to, 
        pw=get.save.workunit.pw,
        minion.workunit=minion.workunit, 
        minion.output=minion.output, 
        minion.output.file.name=minion.output.file.name, 
        client=host.name)
    } else {
      MinionNotifyFailure(url.save.results.to=url.save.results.to,
        pw=get.save.workunit.pw,
        minion.workunit=minion.workunit, 
        client=host.name)
    }
    n.retries <- 0
    minion.output.mysql <- NULL
    minion.output <- NULL
    minion.output.file.name <- NULL
    minion.success <- FALSE
  }
}
Say("Exiting Minion due to lack of work.")
Announce(goodbye.message)
source("http://sheer.ucdavis.edu/svn/software/public/deCruft/deCruft.R")  # must be last
