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

params <- onc_par

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
    shiny::textOutput("parms"),
    shiny::textOutput("init")
)


server <- function(input, output) {

    parset <- shiny::reactive({
      if(input$parset == "onc"){
        return(onc_par)
      }
      if(input$parset == "cal"){
        return(calanus_par)
      }
      return(onc_par)
    })


    output$modplot <- shiny::renderPlot({
        params <- parset()
        parms <- sapply(seq_along(names(params)), function(i){
            input[[names(params)[[i]]]]
        })
        names(parms) <- names(params)

        init2 <- sapply(seq_along(names(init)), function(i){
            input[[names(init)[[i]]]]
        })
        names(init2) <- names(init)

        mod <- deSolve::ode(y = init2, times = times, parms = parms, func = mod)

        plot(mod, xlim = input$xlim)
        plot(mod[, "time"], mod[,"Y"]/mod[,"H"],
             xlim = input$xlim,
             type = "l",
             xlab = "time",
             ylab = "",
             main = "Y/H")

    }, height = 600)



    output$parms <- shiny::renderText({
        params <- parset()
        parms_temp <- sapply(seq_along(names(params)), function(i){
            paste0(names(params)[[i]], " = ", input[[names(params)[[i]]]])
        })
        paste0("parms <- c(", paste0(parms_temp, collapse = ", "), ")")
    })

    output$init <- shiny::renderText({

        init_temp <- sapply(seq_along(names(init)), function(i){
            paste0(names(init)[[i]], " = ", input[[names(init)[[i]]]])
        })
        paste0("init <- c(", paste0(init_temp, collapse = ", "), ")")
    })

    output$parsliders <- shiny::renderUI({
      params <- parset()
      lapply(seq_along(params), function(x, y, i){
          shiny::sliderInput(paste0(y[[i]]),
                    paste0(y[[i]]),
                    min = 0,
                    max = ifelse(x[[i]]<0.01, 0.01,
                                 ifelse(x[[i]] <= 1, 1,
                                        x[[i]]*2)),
                    value = x[[i]]
        )
      }, x = params, y = names(params))
    })
}

# Run the application
shiny::shinyApp(ui = ui, server = server)
