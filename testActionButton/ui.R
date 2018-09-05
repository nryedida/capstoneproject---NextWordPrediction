library(shiny)

ui <- fluidPage(
    titlePanel("Word Prediction"),
    
    sidebarLayout(
        sidebarPanel(
            textInput("caption", "Enter text", "how are"),
            actionButton("go","Go")
        ),
        
        mainPanel(
            h3("Next three words"),
            verbatimTextOutput("nextword")
        )
    )
)
