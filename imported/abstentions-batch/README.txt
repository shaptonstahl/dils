Documentation for batch run of JAGS in R for Monte Carlo comparison
of Clinton, Jackman, and Rivers (CJR) and Rosas, Shomer, and
Haptonstahl (RSH) 1-dimensional IRT models for the analysis of
congressional roll call voting.

__Process_Overview__

The file 'workunits.RData' is created containing a data frame
'workunits'. Each row of this data frame corresponds to a workunit.

A copy of this repository is on a tadano.wustl.edu. The
administrator sets up a table in a MySQL database, then uses
http://tadano.wustl.edu/abstentions/batch/ to add records to the
database table, one record for each workunit.

The computers used to run the simulations, 'slaves', must have JAGS,
R, and R packages 'abind', 'RCurl' (at Omegahat), and 'runjags'
installed.  To run the simulation, just type the following in R:

  source("http://tinyurl.com/ba3pk5")

which forwards to http://tadano.wustl.edu/abstentions/batch/batch.R.
This script loads the libraries and downloads workunits.RData. Then
it calls 'checkout.php' to be assigned a workunit. Workunits are
assigned by looking at all workunits that are not yet completed (not
yet "checked in") and choosing the one that was modified the longest
time ago, then setting the 'checkedout' field to 1 and updating the
modification timestamp.  It runs JAGS on for CJR and RSH on the same
simulated legislature, abstention mechanism, and abstention rate.
Then it uploads the results in an RData file via the page
'save.php'.

The page 'save.php' checks to see if the workunit has been checked
in; if so, it dumps the results; if not, it stores the RData file
and marks the workunit's record in the MySQL table with checkedout=1
and checkedin=1.  RData files are stored as local files on
tadano.wustl.edu, not in the database.

The script batch.R stops if there is an error; if not, it requests a
new work unit and continues.

__Files_and_their_use__

all_models.jags: R source file creating 'models', a list containing
all the JAGS models for CJR and RSH.

batch.R: Main script called by slaves

checkout.php: Called by batch.R to get a workunit assignment; calls
MySQL to get workunit modified longest time ago.

control_functions.php: Helper functions used by other PHP files.

generateWorkunitControl.R: Creates workunits.RData.

index.php: Control and status page for administrators. Shows the
status of all projects and allows adding workunits to the database.

README.txt: this file

save.php: Called by batch.R to save the results of JAGS runs.
Updates the MySQL table to show that the corresponding workunit is
"checkedin".

workunits.RData: Contains data frame 'workunits' which has one row
per workunit. Contains parameters that vary across workunits.
