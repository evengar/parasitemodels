mod_name <- "Anderson & May model B+F+G"
mod <- paRmodels::AM81_BFG

onc_par <- c(
    r = 0.1553797192,
    b = 0.09523809524,
    K = 10,
    v = 0.000888360697,
    g = 0,
    a = 0.0398970399,
    m = 0.3372620127,
    L = 360,
    f = 0.5
)

calanus_par <- list(
  r = 0.107782157552587,
  b = 0.0697821575525873,
  K = 10,
  v = 0.04130139511238,
  g = 0,
  a = 0.03989704,
  m = 0.337262,
  L = 3048.66850321396,
  f = 1
)

times <- 1:999

init <- c(
    H = 1.0,
    Y = 0.0,
    W = 0.1
)

parsetUI <- function(id){
    ns <- NS(id)
    tagList(
        selectInput(ns("parset"), "Set of parameters",
                           c("Oncaea sp.-Blastodinium mangini" = "onc",
                             "C. finmarchicus-B. hyalinum" = "cal"))
    )
}

parsetServer <- function(id){
    moduleServer(
        id,
        function(input, output, session){
            reactive({
                switch(input$parset,
                       onc = return(onc_par),
                       cal = return(calanus_par))
            })
        }
    )
}

initSliderUI <- function(id, values){
    ns <- NS(id)

    lapply(seq_along(values), function(x, y, i){
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

ui <- fluidPage(

    # Application title
    titlePanel(mod_name),

    # Sidebar with a slider input
    sidebarLayout(
        sidebarPanel(
            parsetUI("parset"),
            titlePanel("Parameters"),
            parsliderUI("parslider"),
           width = 3, style = "overflow-y:scroll; max-height: 600px; position:relative;",

           titlePanel("Initial state"),
           initSliderUI("init", init)
           ),

         mainPanel(
            plotOutput("modplot")
        )
    ),
    sliderInput("xlim", "xlim",
                min = 0,
                max = length(times),
                value = c(0,length(times)),
                step = 1),
    checkboxInput("toggle", "Show analytical steady state"),
    textOutput("parms"),
    textOutput("init")
)


server <- function(input, output) {

    # react to input parameter set
    parset <- parsetServer("parset")

    # set up input sliders
    # scaling slider ranges to input size
    parsliderServer("parslider", parset)


    # get current parameters
    parameters <- getParServer("parslider", parset)



    # get current initial values
    initial <- initSliderServer("init", init)


    # setup button for toggling ss visualization
    plotsteady <- reactiveVal(TRUE)
    observeEvent(input$toggle, {
        plotsteady(!plotsteady())
    })

    # get current steady state
    steady <- reactive({
        paRmodels::ss_AM81_BFG(parameters())
    })

    # create visualization
    output$modplot <- renderPlot({
        modl <- deSolve::ode(y = initial(), times = times, parms = parameters(), func = mod)
        modl <- as.data.frame(modl)
        par(mfrow = c(2, 2))

        ss <- steady()

        plot(H ~ time, data = modl, xlim = input$xlim, type = "l",
             ylab = "Total host population H (ind/L)",
             xlab = "Time (days)")
        if(plotsteady()){
            abline(h = ss[1], col = 2, lty = 2)
        }
        plot(Y ~ time, data = modl, xlim = input$xlim, type = "l",
             ylab = "Infected hosts Y (ind/L)",
             xlab = "Time (days)")
        if(plotsteady()){
            abline(h = ss[2], col = 2, lty = 2)
        }
        plot(W ~ time, data = modl, xlim = input$xlim, type = "l",
             ylab = "Free-living parasites W (ind/L)",
             xlab = "Time (days)")
        if(plotsteady()){
            abline(h = ss[3], col = 2, lty = 2)
        }
        plot(modl[, "time"], modl[,"Y"]/modl[,"H"],
             xlim = input$xlim,
             type = "l",
             xlab = "time",
             ylab = "Parasite prevalence Y/H")
        if(plotsteady()){
            abline(h = ss[2]/ss[1], col = 2, lty = 2)
        }

    }, height = 600)



    output$parms <- renderText({
        params <- parameters()
        parmassign <- paste0(names(params), " = ", params)
        paste0("parms <- c(", paste0(parmassign, collapse = ", "), ")")
    })



    output$init <- renderText({
        inits <- initial()
        initassign <- paste0(names(inits), " = ", inits)
        paste0("init <- c(", paste0(initassign, collapse = ", "), ")")
    })


}

# Run the application
shinyApp(ui = ui, server = server)
