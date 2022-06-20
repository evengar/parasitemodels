statePlotUI <- function(id, varnames){
    ns <- NS(id)
    tagList(
        selectInput(ns("xvar"), "X-axis variable", varnames),
        selectInput(ns("yvar"), "Y-axis variable", varnames, selected = varnames[2]),
        plotOutput(ns("stateplot"))
    )
}

statePlotServer <- function(id, reactive_df){
    stopifnot(is.reactive(reactive_df))
    moduleServer(
        id,
        function(input, output, session){

            output$stateplot <- renderPlot({
                x <- reactive_df()[[input$xvar]]
                y <- reactive_df()[[input$yvar]]
                plot(x, y, type = "l",
                     xlab = input$xvar,
                     ylab = input$yvar)
            })

        })
}
