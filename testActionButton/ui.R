library(shiny)

ui <- fluidPage(
    tags$head(
        tags$style(HTML("
            .tabbable > .nav > li > a                  {background-color: aqua;  color:black}
            .tabbable > .nav > li > a[data-value='t1'] {background-color: red;   color:white}
            .tabbable > .nav > li > a[data-value='t2'] {background-color: blue;  color:white}
            .tabbable > .nav > li > a[data-value='t3'] {background-color: green; color:white}
            .tabbable > .nav > li[class=active]    > a {background-color: black; color:white}
                                ")),
    # tags$style(HTML("
    #     .tabs-above > .nav > li[class=active] > a {
    #                     background-color: #000;
    #                     color: #FFF;
    #                     }")),    
        tags$style(HTML('
                        #go    {
                            background-color:orange
                        }
                         #sidebar {
                            background-color: White;
                        }

                        ')
                   )
    ),
    # Application title
    titlePanel("Capstone Project"),
    
##################################################################
    # Sidebar with a slider input for number of bins
    tabsetPanel(type = "tabs",
                tabPanel("Next Word Predictor",
                         sidebarLayout(
                             sidebarPanel(id="sidebar",
                                 div(span("Please enter one or more words in the text box and click on the Go button", style = " font-size:1.1em")),
                                 width = 2

                                 # textOutput("Instruction", "Please enter text")
                                 # actionButton("go","Go")
                             ),

                             mainPanel(
                                 h3(textInput("caption", "Please enter text")),
                                 width = 10,
                                 actionButton("go","Go"),
                                 br(),
                                 br(),
                                 br(),
                                 h3("Next word"),
                                 div(span(textOutput("nextword"), style = "color:blue; font-size:1.5em "))
                             )
                         )
                ),

                tabPanel("About", h1("About"))
    )
##################################################################

    
        
# ################################################################
#   titlePanel("Word Prediction"),
# 
#     sidebarLayout(
#         sidebarPanel(
#             div(span("Please enter one or more words in the text panel on the right and click on Predict button", style = "color:blue; font-size:1.25em")),
#             width = 2
# 
#             # textOutput("Instruction", "Please enter text")
#             # actionButton("go","Go")
#         ),
# 
#         mainPanel(
#             textInput("caption", "Please enter text"),
#             width = 2,
#             actionButton("go","Predict"),
#             br(),
#             br(),
#             br(),
#             h4("Next word"),
#             div(span(textOutput("nextword"), style = "color:blue"))
#         )
#     )
# ################################################################
)
