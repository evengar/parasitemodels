modelUI <- function(id){
    ns <- NS(id)
    tagList(
        selectInput(ns("modelt"), "Model type",
                    c("Regular" = "reg",
                      "Seasonal" = "seas"))
    )
}

modelServer <- function(id, initial, parameters, mod, timesteps, mod_name){

    moduleServer(
        id,
        function(input, output, session){
            reactive({
                if(input$modelt == "seas"){
                    mod_seas <- paste0(mod_name, "_seasonal")
                    mod <- eval(parse(text = mod_seas))
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

parsetUI <- function(id, inputpars, pars_is_list){
    ns <- NS(id)
    if (pars_is_list){
        parameter_tags <- setNames(gsub(" ", "", names(parameters)), names(parameters))
    }else{
        parameter_tags <- c("User parameters" = "inp")
    }
    tagList(
        selectInput(ns("parset"), "Set of parameters",
                    parameter_tags)
    )
}

parsetServer <- function(id, inputpars, pars_is_list){
    moduleServer(
        id,
        function(input, output, session){
            reactive({
                #browser()
                if(pars_is_list){
                    pars <- inputpars[[which(gsub(" ", "", names(parameters)) == input$parset)]]
                }else{
                    pars <- inputpars
                }

                if(input$modelt == "seas" & is.na(pars["Ks"])) pars <- c(pars, Ks = 0.5)
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
            step = if (x[[i]] != 0) min(0.01, x[[i]]) else 0.01)
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
                                max = ifelse(x[[i]]<0.01 & x[[i]] > 0, x[[i]]*10,
                                             ifelse(x[[i]] <= 1, 1,
                                                    x[[i]]*2)),
                                value = x[[i]],
                                step = if(x[[i]] > 1) 0.1 else NULL
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

outbreakServer <- function(id, params){
    moduleServer(
        id,
        function(input, output, session){
            reactive({outbreak_conditions(input$xvar, input$yvar, params())})

        }
    )
}
