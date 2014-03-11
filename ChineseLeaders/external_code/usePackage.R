# usePackage
#
# Author: Stephen R. Haptonstahl (srh@haptonstahl.org)
# Source: https://raw.github.com/shaptonstahl/R
#
# Call with:
#   source("http://www.haptonstahl.org/R/usePackage/usePackage.R")

usePackage <- function(package.name) invisible(UsePackage(package.name))

UsePackage <- function(package.name) {
  # Given a character string of the name of a package,
  # installs if not already installed and loads the package.
  #
  # Author: Stephen R. Haptonstahl (srh@haptonstahl.org)
  # Load using: source("http://www.haptonstahl.org/R/usePackage/usePackage.R")
  old.warn.level <- as.numeric(options('warn'))
  options(warn=-1)
  require.result <- require(package.name, character.only=T)
  options(warn=old.warn.level)
  if( !require.result ) {
    install.packages(package.name)
    require.result <- require(package.name, character.only=T)
  }
  invisible(require.result)
}
