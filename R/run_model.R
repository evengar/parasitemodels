#' Run shiny app for model B + F + G from Anderson & May (1981).
#'
#' Run model B + F + G with different default parameter sets
#' and the possibility to change each parameter interactively.
#'
#' @export
#'
run_model <- function() {
    appDir <- system.file("shiny-examples", "BFG-app", package = "paRmodels")
    if (appDir == "") {
        stop("Could not find example directory. Try re-installing `paRmodels`.", call. = FALSE)
    }

    shiny::runApp(appDir, display.mode = "normal")
}
