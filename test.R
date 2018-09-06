# single trigram
predictWords("Learn tips how to easy earn money")

trifreq[grep("",trifreq$feature),1],3)
bifreq[grep("",bifreq$feature),1],3)
##################################
library(shiny)
runExample("02_text")


library(shiny)
runExample("01_hello")

## Only run examples in interactive R sessions
if (interactive()) {
    
    ui <- fluidPage(
        textInput("caption", "Enter text", "how are"),
        verbatimTextOutput("value")
    )
    server <- function(input, output) {
        output$value <- renderText({ input$caption })
    }
    shinyApp(ui, server)
}

####################################
shinyApp(
    ui = fluidPage(
        column(4,
               numericInput("x", "Value", 5),
               br(),
               actionButton("button", "Show")
        ),
        column(8, tableOutput("table"))
    ),
    server = function(input, output) {
        # Take an action every time button is pressed;
        # here, we just print a message to the console
        observeEvent(input$button, {
            cat("Showing", input$x, "rows\n")
        })
        # Take a reactive dependency on input$button, but
        # not on any of the stuff inside the function
        df <- eventReactive(input$button, {
            head(cars, input$x)
        })
        output$table <- renderTable({
            df()
        })
    }
)