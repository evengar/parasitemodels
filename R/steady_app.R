#' @export
#'
steady_app <- function(pars, mod_name = NULL, mod_str = deparse(substitute(mod)), ...){
    if(is.null(mod_name)){
        mod_name <- mod_str
    }
    ui <- fluidPage(

        tags$head(tags$style(HTML("
                               body {
                                  width: 100% !important;
                                  max-width: 100% !important;
                               }

                               "))),

        # Application title
        titlePanel("Threshold for outbreak"),

        # Sidebar with a slider input
        sidebarLayout(
            sidebarPanel(
                modelUI("parset"),
                parsetUI("parset"),
                titlePanel("Parameters"),
                parsliderUI("parslider"),
                width = 3, style = "overflow-y:scroll; max-height: 600px; position:relative;",

                titlePanel("Initial state")
            ),
            mainPanel("Time series",
                      plotOutput("modplot")

            )
        )
    )

    server <- function(input, output){
        # react to input parameter set
        parset <- parsetServer("parset", pars)

        # set up input sliders
        # scaling slider ranges to input size
        parsliderServer("parslider", parset)


        # get current parameters
        parameters <- getParServer("parslider", parset)

    }

    shinyApp(ui = ui, server = server, options = list(height = 2000))
}
