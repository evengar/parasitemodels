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
                switch(input$parset,
                       inp = return(inputpars),
                       onc = return(oncaea_par),
                       cal = return(calanus_par))
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
            max = 5,
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
