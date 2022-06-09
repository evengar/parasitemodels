

#' Get R0 from model 1 from Garvang et al. (2022)
#'
#' @param pars A named list or named vector of parameters
#'
#' @return A double
#' @export
#'
#' @examples
#'
#' onc_par <- c(
#'     r = 0.1553797192,
#'     b = 0.09523809524,
#'     K = 10,
#'     v = 0.000888360697,
#'     g = 0,
#'     a = 0.0398970399,
#'     m = 0.3372620127,
#'     L = 360,
#'     f = 1
#'     )
#'
#' r0(onc_par)
#'
r0 <- function(pars){
    with(as.list(pars), {
        (L - 1) * ((r - b) / r) * ((v * K)/m)
    })
}
