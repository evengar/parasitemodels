#' @export
get_R0_root <- function(pars, variable, variable_name){
    pars <- as.list(pars)
    pars[[variable_name]] <- variable
    get_R0(pars) - 1
}

#' Find bifurcation point where R0 becomes 1 by varying a single parameter
#'
#' @inheritParams get_R0
#'
#' @param variable a named numeric vector of length 2, containing min and max values
#' of a single varying parameter
#'
#' @param variable_name name of `variable`
#'
#' @return a list containing at least `root`, the value of variable where
#' R0 equals 1
#' @export
#'
root_var <- function(pars,
                     variable,
                     variable_name = names(variable)){
    # make sure variable name argument is evaluated since we overwrite later
    force(variable_name)
    names(variable) <- NULL # avoid confusing list naming

    pars <- as.list(pars)   # make into list to make room for varying pars

    pars[variable_name] <- variable
    variable <- unlist(variable) # uniroot takes vector as arguments
    R0 <- get_R0(pars) # R0 for min and max of variable (rest of pars are recycled)

    #Only get root if R0 increases through 1
    # otherwise return root = NA
    if(R0[1] < 1 & R0[2] > 1){
        result <- stats::uniroot(get_R0_root,
                          interval = variable,
                          pars = pars,
                          variable_name = variable_name)
    }else{
        result <- list(root = NA)
    }
    return(result)
}

#' Vary multiple variables and find bifurcation point
#'
#' @inheritParams root_var
#' @param grid A varying parameter or a grid of different combinations of
#' parameters
#' @param iter_name the name(s) of the parameters in `grid`
#'
#' @return
#' @export
#'
#' @examples
root_multi <- function(pars,
                       variable,
                       grid,
                       variable_name = names(variable),
                       iter_name = names(grid)){

    results <- list()
    temppars <- as.list(pars)
    for (i in 1:nrow(grid)){
        temppars[iter_name] <- grid[i,]
        results[[i]] <- root_var(
            temppars, variable, variable_name
        )
        results[[i]][iter_name] <- grid[i,]
    }

    results_mat <- do.call(rbind, lapply(
        results, function(x){
            vec <- NULL
            vec[iter_name] <- x[iter_name]
            vec[variable_name] <- x$root
            return(unlist(vec))
        }))
    return(as.data.frame(results_mat))
}
