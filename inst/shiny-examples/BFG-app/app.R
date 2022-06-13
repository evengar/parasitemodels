mod_name <- "Anderson & May model B+F+G"
mod <- AM81_BFG

onc_par <- c(
    r = 0.1553797192,
    b = 0.09523809524,
    K = 10,
    v = 0.000888360697,
    g = 0,
    a = 0.0398970399,
    m = 0.3372620127,
    L = 360,
    f = 0.5,
    Ks = 0.99
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

param_names <- names(onc_par)

times <- 1:999

init <- c(
    H = 1.0,
    Y = 0.0,
    W = 0.1
)



ui <- shiny::fluidPage(

    # Application title
    shiny::titlePanel(mod_name),

    # Sidebar with a slider input
    shiny::sidebarLayout(
        shiny::sidebarPanel(
            shiny::selectInput("season", "Model type",
                               c("Regular", "Seasonal")),
            shiny::selectInput("parset", "Set of parameters",
                      c("Oncaea sp.-Blastodinium mangini" = "onc",
                        "C. finmarchicus-B. hyalinum" = "cal")),
            shiny::titlePanel("Parameters"),
            shiny::uiOutput("parsliders"),
           width = 3, style = "overflow-y:scroll; max-height: 600px; position:relative;",

           shiny::titlePanel("Initial state"),
          lapply(seq_along(init), function(x, y, i){
              shiny::sliderInput(paste0(y[[i]]),
                          paste0(y[[i]]),
                          min = 0,
                          max = 5,
                          value = x[[i]],
                          step = 0.1)
          },
            x = init, y = names(init))),

        shiny:: mainPanel(
            plotOutput("modplot")
        )
    ),
    shiny::sliderInput("xlim", "xlim",
                min = 0,
                max = length(times),
                value = c(0,length(times)),
                step = 1),
    shiny::checkboxInput("toggle", "Show analytical steady state"),
    shiny::textOutput("parms"),
    shiny::textOutput("init")
)


server <- function(input, output) {

    # react to input parameter set
    parset <- shiny::reactive({
        switch(input$parset,
               onc = return(onc_par),
               cal = return(calanus_par))
    })

    # set up input sliders
    # scaling slider ranges to input size
    output$parsliders <- shiny::renderUI({
        params <- parset()
        lapply(seq_along(params), function(x, y, i){
            shiny::sliderInput(paste0(y[[i]]),
                               paste0(y[[i]]),
                               min = 0,
                               max = ifelse(x[[i]]<0.01 & x[[i]] > 0, 0.01,
                                            ifelse(x[[i]] <= 1, 1,
                                                   x[[i]]*2)),
                               value = x[[i]]
            )
        }, x = params, y = names(params))
    })


    # get current parameters
    parameters <- shiny::reactive({
        current <- sapply(seq_along(param_names), function(i){
            input[[param_names[[i]]]]
        })
        setNames(current, param_names)
    })



    # get current initial values
    initial <- shiny::reactive({
        current <- sapply(seq_along(names(init)), function(i){
            input[[names(init)[[i]]]]
        })
        setNames(current, names(init))
    })


    # setup button for toggling ss visualization
    plotsteady <- shiny::reactiveVal(TRUE)
    shiny::observeEvent(input$toggle, {
        plotsteady(!plotsteady())
    })

    # get current steady state
    steady <- shiny::reactive({
        ss_AM81_BFG(parameters())
    })

    # create visualization
    output$modplot <- shiny::renderPlot({
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



    output$parms <- shiny::renderText({
        params <- parameters()
        parmassign <- paste0(names(params), " = ", params)
        paste0("parms <- c(", paste0(parmassign, collapse = ", "), ")")
    })



    output$init <- shiny::renderText({
        inits <- initial()
        initassign <- paste0(names(inits), " = ", inits)
        paste0("init <- c(", paste0(initassign, collapse = ", "), ")")
    })


}

# Run the application
shiny::shinyApp(ui = ui, server = server)
