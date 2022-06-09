AM81_BFG <- function(t, x, parms){
    with(as.list(c(parms,x)), {

        dY = v*W*(H - Y) - (a + b + g)*Y
        dW = L*(a + b + g)*Y - (m + v*H)*W
        dH = (r*(1 - (H/K)) - b)*H - (a + f*r*(1 - (H/K)))*Y

        return(list(c(dH, dY, dW)))

    })
}
