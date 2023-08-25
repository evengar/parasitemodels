#' @export
run_app <- function(mod, pars, init, steady_fun = ss_AM81_BFG_biomass, mod_name = NULL, mod_str = deparse(substitute(mod)), ...){

    if(is.null(mod_name)){
        mod_name <- mod_str
    }
    pars_is_list <- inherits(pars, "list")

    ui <- fluidPage(

        tags$head(tags$style(HTML("
                               body {
                                  width: 100% !important;
                                  max-width: 100% !important;
                               }

                               "))),

        # Application title
        titlePanel(mod_name),

        # Sidebar with a slider input
        sidebarLayout(
            sidebarPanel(
                modelUI("parset"),
                parsetUI("parset", pars, pars_is_list),
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
        parset <- parsetServer("parset", pars, pars_is_list)

        # set up input sliders
        # scaling slider ranges to input size
        parsliderServer("parslider", parset)


        # get current parameters
        parameters <- getParServer("parslider", parset)



        # get current initial values
        initial <- initSliderServer("init", init)

        timesteps <- reactive({
            input$time
        })


        # setup button for toggling ss visualization
        plotsteady <- reactiveVal(TRUE)
        observeEvent(input$toggle, {
            plotsteady(!plotsteady())
        })

        # get current steady state
        steady <- reactive({
            steady_fun(parameters())
        })

        modl <- modelServer("parset", initial, parameters, mod, timesteps, mod_str)

        # create visualization
        output$modplot <- renderPlot({



            if(plotsteady())
                ss <- steady()
            plotdata <- modl()

            # set up plot grid

            nstates <- ncol(plotdata) - 1
            par(mfrow = c(ceiling(nstates / 2), 2))
            #browser()

            for (i in 2:ncol(plotdata)){
                #browser()
                state <- names(plotdata)[i]
                plot(plotdata[,"time"], plotdata[,i],
                     type = "l",
                     xlab = "Time",
                     ylab = state)

                if(plotsteady()){
                    if (state %in% names(ss)){
                        abline(h = ss[state], col = 2, lty = 2)
                    }
                }
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
    shinyApp(ui = ui, server = server, options = list(height = 2000))

}
