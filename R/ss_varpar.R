#' Utility for varying model parameters
#'
#' @inheritParams ss_varpar
#' @export
new_par <- function(var, ref) {
    non.var <- !(names(ref) %in% names(var))
    return(data.frame(var, ref[non.var]))
}


#' Get steady states from a range of parameter values
#'
#' @param var data.frame or list of varying parameters
#' @param ref named vector of parameters
#' @param ss_fun function for calculating steady state
#'
#' @return A data.frame with one row for each variation in parameters
#' @export
#'
ss_varpar <- function(var, ref, ss_fun = ss_AM81_BFG){

    pars <- new_par(var, ref)
    ss <- data.frame(var, H=NA, Y=NA, W=NA)

    for (i in 1:nrow(pars)) {
        par.i <- unlist(pars[i, ])
        ss[i, (ncol(ss) - 2):ncol(ss)] <- ss_fun(par.i)
    }
    return(ss)
}
