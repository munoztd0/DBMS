source("renv/activate.R")


q <- function(save = "no", status = 0, runLast = TRUE){

.Internal(quit(save, status, runLast))

#<environment: namespace:base>

}

q <- list()

class(q) <- 'exiter'

print.exiter <- function(exiterObject){ q() }

