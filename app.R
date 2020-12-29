library(magrittr)
library(shiny)
library(readr)
library(skimr)

ui <- fluidPage(

    # Application title
    titlePanel("Shiny Data Explorer"),

    sidebarLayout(
        sidebarPanel(
            # to upload files
            fileInput("file", "Choose CSV File", accept = ".csv"),
            checkboxInput("header", label = "Header", value = 1),
            actionButton("go", label = "Run")

        ),
        mainPanel(
            # to display summary
            verbatimTextOutput("out", placeholder = FALSE)
        )
    )
)

server <- function(input, output) {
    
    # create a dataframe react to event 'button'
    df <- eventReactive(input$go, {
        uploadedFile <- input$file
        read_csv(uploadedFile$datapath, col_names = input$header)
    })
    
    output$out <- renderPrint({
        
        # skim through data
        skimr::skim(df())
        
    })
    
}

shinyApp(ui = ui, server = server)
