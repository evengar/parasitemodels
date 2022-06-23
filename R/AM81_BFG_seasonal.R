#' Seasonal implementation of the BFG model
#'
#' @inheritParams AM81_BFG
#' @inherit AM81_BFG return description

#' @export

AM81_BFG_seasonal <- function(t, x, parms){
    with(as.list(c(parms,x)), {

        # K depends on day of year
        # bottom on first day, peak on day 365/2
        Kt = K * (1 + Ks * -cos(2 * pi * t/365))

        dY = v*W*(H - Y) - (a + b + g)*Y
        dW = L*(a + b + g)*Y - (m + v*H)*W
        dH = (r*(1 - (H/Kt)) - b)*H - (a + f*r*(1 - (H/Kt)))*Y

        return(list(c(dH, dY, dW)))

    })
}
