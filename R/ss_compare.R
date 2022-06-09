#' Compare analytical and numeric steady state solutions
#'
#' Uses a user-input function for solving a set of ODEs analytically, and
#' rootSolve::steady for calculating a numerical solution. Returns both results
#' for further processing.
#'
#' @inheritParams rootSolve::steady
#' @param ss_func Function for computing analytical steady state solution
#' @param ... Additional arguments passed to rootSolve::steady
#'
#' @return A list of two named numeric vectors, containing the analytical (anl)
#' and numerical (num) solutions, respectively.
#' @export

ss_compare <- function(y, func, ss_func, parms, method = "runsteady", ...){
    ss_analytical <- ss_func(parms)
    ss_numerical <- rootSolve::steady(y = y, func = func, parms = parms, method = method, ...)
    return(list(anl = unlist(ss_analytical), num = ss_numerical$y))
}
