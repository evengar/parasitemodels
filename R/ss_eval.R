#' Evaluate if two steady state solutions are equal
#'
#' Compares the two elements of the output of ss_compare and uses all.equal to
#' check if they are equal. The tolerance is set lower than the default for all.equal
#' because slight deviations are expected. The function should in theory work for any list of length 2.
#'
#' @param ss_comp A list of two vectors to be compared, typically the output of ss_compare()
#'
#' @param tolerance numeric ≥ 0. Differences smaller than tolerance are not reported. The default value is set larger
#' than the regular default for all.equal.
#'
#' @return A boolean value indicating if solutions are equal
#' @export
ss_eval <- function(ss_comp, tolerance = 1e-4){
    stopifnot(length(ss_comp) == 2)
    isTRUE(all.equal(ss_comp[[1]], ss_comp[[2]], tolerance = tolerance))
}
