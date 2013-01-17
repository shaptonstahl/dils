#' Generate simulated DILS data
#' 
#' no missing data
#' columns are grouped binary and continuous

source("http://www.haptonstahl.org/R/Decruft/Decruft.R")
source("http://www.haptonstahl.org/R/RollCallFunctions/RollCallFunctions.R")

is.non.negative <- function(x) ifelse(x >= 0, 1, 0)

true.parameters <- list(n.links=100)
true.parameters$n.binary.vars <- 5
true.parameters$n.continuous.vars <- 5
true.parameters$true.dils <- runif(true.parameters$n.links)

true.parameters$true.beta <- c(runif(true.parameters$n.binary.vars),
               2*runif(true.parameters$n.continuous.vars))
true.parameters$true.alpha <- runif(true.parameters$n.binary.vars + true.parameters$n.continuous.vars)

true.parameters$true.all.ystar <- do.call("rbind",
                             lapply(1:true.parameters$n.links, 
                                    function(i.link) 
                                      return( with(true.parameters, true.beta * true.dils[i.link] - true.alpha) )
                                    ))

# y is the only observed data
y <- true.parameters$true.all.ystar
y[,1:true.parameters$n.binary.vars] <- is.non.negative(y[,1:true.parameters$n.binary.vars])  # score binary response as 1 if ystar >= 0, 0 otherwise

saveRDS(true.parameters, "generated_true_parameters.rds")
saveRDS(y, "generated_y.rds")