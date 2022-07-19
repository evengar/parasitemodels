modelUI <- function(id){
    ns <- NS(id)
    tagList(
        selectInput(ns("modelt"), "Model type",
                    c("Regular" = "reg",
                      "Seasonal" = "seas"))
    )
}

modelServer <- function(id, initial, parameters, mod, timesteps){

    moduleServer(
        id,
        function(input, output, session){
            reactive({
                if(input$modelt == "seas"){
                    mod <- AM81_BFG_seasonal
                }

                #browser()
                modl <- deSolve::ode(y = initial(), times = 1:timesteps(), parms = parameters(), func = mod)

                modl <- as.data.frame(modl)
                modl$prevalence <- modl$Y / modl$H
                modl
                #browser()
            })
        }
    )
}

parsetUI <- function(id){
    ns <- NS(id)
    tagList(
        selectInput(ns("parset"), "Set of parameters",
                    c("User parameters" = "inp",
                      "Oncaea sp.-Blastodinium mangini" = "onc",
                      "C. finmarchicus-B. hyalinum" = "cal"))
    )
}

parsetServer <- function(id, inputpars){
    moduleServer(
        id,
        function(input, output, session){
            reactive({
                #browser()
                pars <- switch(input$parset,
                       inp = inputpars,
                       onc = oncaea_par,
                       cal = calanus_par)
                if(input$modelt == "seas") pars <- c(pars, Ks = 0.5)
                return(pars)
            })
        }
    )
}

initSliderUI <- function(id, init){
    ns <- NS(id)

    lapply(seq_along(init), function(x, y, i){
        sliderInput(
            ns(paste0(y[[i]])),
            paste0(y[[i]]),
            min = 0,
            max = max(5, x[[i]] * 2),
            value = x[[i]],
            step = 0.1)
    },
    x = init, y = names(init))
}

initSliderServer <- function(id, init){
    moduleServer(
        id,
        function(input, output, session){
            reactive({
                current <- sapply(seq_along(names(init)), function(i){
                    input[[names(init)[[i]]]]
                })
                setNames(current, names(init))
            })
        }
    )

}

parsliderUI <- function(id){
    ns <- NS(id)
    tagList(
        uiOutput(ns("parsliders"))
    )
}

parsliderServer <- function(id, params){
    moduleServer(
        id,
        function(input, output, session){
            ns <- session$ns
            output$parsliders <- renderUI({
                lapply(seq_along(params()), function(x, y, i){
                    sliderInput(ns(paste0(y[[i]])),
                                paste0(y[[i]]),
                                min = 0,
                                max = ifelse(x[[i]]<0.01 & x[[i]] > 0, 0.01,
                                             ifelse(x[[i]] <= 1, 1,
                                                    x[[i]]*2)),
                                value = x[[i]]
                    )
                }, x = params(), y = names(params()))
            })
        }
    )
}

getParServer <- function(id, params){
    moduleServer(
        id,
        function(input, output, session){
            reactive({
                current <- sapply(seq_along(names(params())), function(i){
                    input[[names(params())[[i]]]]
                })
                setNames(current, names(params()))
            })
        }
    )
}
