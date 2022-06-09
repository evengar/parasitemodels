#' Model B + F + G from Anderson & May (1981)
#'
#' A function to be passed to a `deSolve` ODE solver. Not for standalone use!
#'
#' @param t current time point in the intergration
#' @param x current estimate of the variables in the ODE system
#' @param parms parameters of the model
#'
#' @return
#' @export
#'
#' @examples
AM81_BFG <- function(t, x, parms){
    with(as.list(c(parms,x)), {

        dY = v*W*(H - Y) - (a + b + g)*Y
        dW = L*(a + b + g)*Y - (m + v*H)*W
        dH = (r*(1 - (H/K)) - b)*H - (a + f*r*(1 - (H/K)))*Y

        return(list(c(dH, dY, dW)))

    })
}
