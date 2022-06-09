get_R0 <- function(pars){
    with(as.list(pars), {
        (L - 1) * ((r - b) / r) * ((v * K)/m)
    })
}
