#' Model B + F + G from Anderson & May (1981)
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
#' )
#' init <- c(
#'     H = 0.1,
#'     Y = 0.0,
#'     W = 0.000001
#' )
#' times <- 1:1000
#'
#' # pass function as `func` argument to deSolve::ode()
#'
#' deSolve::ode(parms = onc_par, times = times, y = init, func = AM81_BFG)
#'
#'
AM81_BFG <- function(t, x, parms){
    with(as.list(c(parms,x)), {

        dY = v*W*(H - Y) - (a + b + g)*Y
        dW = L*(a + b + g)*Y - (m + v*H)*W
        dH = (r*(1 - (H/K)) - b)*H - (a + f*r*(1 - (H/K)))*Y

        return(list(c(dH, dY, dW)))

    })
}
