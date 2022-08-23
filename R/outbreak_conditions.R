#' @export
outbreak_conditions <- function(var1_name, var2_name, pars){
    var1 <- pars[var1_name]
    var2 <- pars[var2_name]

    var1_range <- list(c(1e-6, ifelse(var1 < 1, 1, var1 * 10)))
    names(var1_range) <- var1_name

    var2_seq <- data.frame(var = seq(1e-6, ifelse(var2 < 1, 1, var2 * 10), length.out = 1000))
    names(var2_seq) <- var2_name

    root_multi(pars, var1_range, var2_seq)
}
