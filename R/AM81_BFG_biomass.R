#' Modeling host and parasite biomass, using a model modified from Anderson & May (1981)
#'
#' A function to be passed to a `deSolve` ODE solver. Not for standalone use!
#'
#' @param t current time point in the intergration
#' @param x current estimate of the variables in the ODE system
#' @param parms parameters of the model
#'
#' @return list of derivatives of state variables
#' @export
#'
AM81_BFG_biomass <- function(t, x, parms){
    with(as.list(c(parms,x)), {

        dY = (q)*v*W*(H - Y) - (a + b + g)*Y
        dW = (q^-1)*L*(a + b + g)*Y - (m + v*H)*W
        dH = (r*(1 - (H/K)) - b)*H - (a + f*r*(1 - (H/K)))*Y

        return(list(c(dH, dY, dW)))

    })
}
