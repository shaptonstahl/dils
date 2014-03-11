# Decruft
#
# Author: Stephen R. Haptonstahl (srh@haptonstahl.org)
# Source: https://raw.github.com/shaptonstahl/R
#
# Call with:
#   source("http://www.haptonstahl.org/R/Decruft/Decruft.R")

cat("\nDecruft by Stephen Haptonstahl (srh@haptonstahl.org)\n\n")
rm(list=ls(all.names=TRUE, envir=.GlobalEnv), envir=.GlobalEnv)  # clears *all* objects including visible and hidden environments

max.fails <- 500

GetNondefaultLoadedPackages <- function() {
  setdiff(loadedNamespaces(), c("base", "tools", options()$defaultPackages))
}

fail.counter <- 0
while( length(pkgs.to.remove <- GetNondefaultLoadedPackages()) > 0 ) {
  res <- tryCatch( unloadNamespace(sample(pkgs.to.remove, 1)),
                   error=function(e) e,
                   warning=function(w) w)
  if( !is.null(res) ) fail.counter <- fail.counter + 1
  if(fail.counter >= max.fails) break
}

if(fail.counter >= max.fails) {
  cat("Unable to remove all package environments from the search() path.",
      "You may want to restart R to guarantee a clean session.\n\n", 
      sep="\n")
} else {
  cat("All packages unloaded.\n\n")
}

rm(list=ls(all.names=TRUE, envir=.GlobalEnv), envir=.GlobalEnv)  # clears *all* objects including visible and hidden environments
cat("All objects deleted, including hidden package environments.\n")
