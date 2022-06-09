#' Analytical steady state of model B+F+G
#'
#' Get analytical steady state of model B+F+G from Anderson & May (1981) by solving a 2nd degree polynomial.
#' If the polynomial has comlpex roots, the function will return the parasite-free
#' steady state.
#'
#' @param par parameters of the model
#'
#' @return A named numeric vector with the steady state of the three state variables H, Y and W
#' @export
#'
#' @examples
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
#'
#' ss_AM81_BFG(onc_par)
ss_AM81_BFG <- function(par) {

    z <- with(as.list(par), {
        a0 <- -m * (a + f * r) / (L * v * K)
        a1 <- -(r * (1 - f * (1 + (m/(K*v*L)) - (1/L))) - b - a * (1 - (1/L)))
        a2 <- r * (1 + f * ((1/L) - 1))

        polyroot(c(a0, a1, a2))
    })

    x <- with(as.list(par), ((r - b) / r)) # Parasite-free solution

    if (all(abs(Im(z)) < sqrt(.Machine$double.eps))) {
        z <- Re(z)
        in.unit <- ((z >= 0) & (z <= x))
        if (any(in.unit)) {
            x <- z[in.unit][1]
        }
    }

    H <- with(as.list(par), K * x)
    Y <- with(as.list(par), (K / (a + f * r * (1-x))) * (r * (1 - x) - b) * x)
    W <- with(as.list(par), (L * (a + b + g)) * Y / (m + v * H))

    return(c(H=H, Y=Y, W=W))
}
