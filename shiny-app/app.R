library(shiny)
library(bslib)
library(shinycssloaders) #optional rn

options(spinner.type = 8, spinner.color ="#6990EE")

ui <- page_sidebar(


  #theme
  sidebar = sidebar(
    #samplesize
    selectInput(

    ),
    #noiselevel
    selectInput(

    ),
    #random seed
    selectInput(

    ),
    #regression model choice (polynomial or kNN regression)
    selectInput(

    ),
    #Model complexity parameter (polynomail degree or number of neighbours k)
    selectInput(

    ),
    #option to download simulated data (current)
    downloadButton(

    )
    #extras for later


  ),
  #for specific selected inputs control
  conditionalPanel(

  ), #...




  #outputs later
)

server <- function(input, output, session){
  source(file.path("server-plots.R"), local = TRUE)$ value
}

shinyApp(ui=ui, server=server)