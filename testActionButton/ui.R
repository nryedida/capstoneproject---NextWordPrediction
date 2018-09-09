library(shiny)

ui <- fluidPage(
  titlePanel("Word Prediction"),
    
    sidebarLayout(
        sidebarPanel(
            div(span("Please enter one or more words in the text panel on the right and click on Predict button", style = "color:blue; font-size:1.25em")),
            width = 2

            # textOutput("Instruction", "Please enter text")
            # actionButton("go","Go")
        ),
        
        mainPanel(
            textInput("caption", "Please enter text"),
            width = 2,
            actionButton("go","Predict"),
            br(),
            br(),
            br(),
            h4("Next word"),
            div(span(textOutput("nextword"), style = "color:blue"))
        )
    )
)