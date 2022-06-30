run_app <- function(mod, pars, init, mod_name = deparse(substitute(mod)), ...){


    ui <- fluidPage(

        # Application title
        titlePanel(mod_name),

        # Sidebar with a slider input
        sidebarLayout(
            sidebarPanel(
                modelUI("parset"),
                parsetUI("parset"),
                titlePanel("Parameters"),
                parsliderUI("parslider"),
                width = 3, style = "overflow-y:scroll; max-height: 600px; position:relative;",

                titlePanel("Initial state"),
                initSliderUI("init", init)
            ),
            mainPanel(
                tabsetPanel(
                    tabPanel("Time series",
                             plotOutput("modplot")
                    ),
                    tabPanel("Phase plane",
                             statePlotUI("stateplot", c(names(init), "prevalence"))

                    )
            )

            )
        ),
        sliderInput("time", "Number of time steps",
                    min = 1000,
                    max = 10000,
                    value = 1000,
                    step = 100),
        checkboxInput("toggle", "Show analytical steady state"),
        textOutput("parms"),
        textOutput("init")
    )


    server <- function(input, output) {

        # react to input parameter set
        parset <- parsetServer("parset", pars)

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

        modl <- modelServer("parset", initial, parameters, mod)

        # create visualization
        output$modplot <- renderPlot({

            par(mfrow = c(2, 2))

            ss <- steady()

            plot(H ~ time, data = modl(), type = "l",
                 ylab = "Total host population H (ind/L)",
                 xlab = "Time (days)")
            if(plotsteady()){
                abline(h = ss[1], col = 2, lty = 2)
            }
            plot(Y ~ time, data = modl(), type = "l",
                 ylab = "Infected hosts Y (ind/L)",
                 xlab = "Time (days)")
            if(plotsteady()){
                abline(h = ss[2], col = 2, lty = 2)
            }
            plot(W ~ time, data = modl(),  type = "l",
                 ylab = "Free-living parasites W (ind/L)",
                 xlab = "Time (days)")
            if(plotsteady()){
                abline(h = ss[3], col = 2, lty = 2)
            }
            plot(prevalence ~ time, data = modl(),
                 type = "l",
                 xlab = "time",
                 ylab = "Parasite prevalence Y/H")
            if(plotsteady()){
                abline(h = ss[2]/ss[1], col = 2, lty = 2)
            }

        }, height = 600)

        statePlotServer("stateplot", modl)



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

}
