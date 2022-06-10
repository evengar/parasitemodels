#' Get difference between analytical and numerical solution
#'
#' For evaluating your analytical solution. Takes the output of ss_compare
#' and finds subtracts the numerical from the analytical solution. Should work
#' with any list of length 2.
#'
#' @inheritParams ss_eval
#'
#' @return A vector of differences between the analytical and the numerical
#' steady state solutions
#' @export
ss_diff <- function(ss_comp){
    stopifnot(length(ss_comp) == 2)
    ss_comp[[1]] - ss_comp[[2]]
}
