statePlotUI <- function(id, init){
    ns <- NS(id)
    vars <- c(names(init), "prevalence")
    tagList(
        selectInput(ns("xvar"), "X-axis variable", vars),
        selectInput(ns("yvar"), "Y-axis variable", vars, selected = "Y"),
        plotOutput(ns("stateplot"))
    )
}

statePlotServer <- function(id, modl){

    moduleServer(
        id,
        function(input, output, session){

            output$stateplot <- renderPlot({
                x <- modl()[[input$xvar]]
                y <- modl()[[input$yvar]]
                plot(x, y, type = "l",
                     xlab = input$xvar,
                     ylab = input$yvar)
            })

        })
}
