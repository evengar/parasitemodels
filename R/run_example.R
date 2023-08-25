#' Run shiny app for model B + F + G from Anderson & May (1981).
#'
#' Run model B + F + G with different default parameter sets
#' and the possibility to change each parameter interactively.
#'
#' @export
#'
run_example <- function() {
    init <- c(
        H = 0.05,
        Y = 0,
        W = 0.005
    )

    run_app(AM81_BFG_biomass, parameters, init)
}
