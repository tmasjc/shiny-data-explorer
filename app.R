require(shiny)
library(shinythemes)
require(magrittr)
require(readr)
require(skimr)
require(purrr)
require(glue)
require(rlang)


ui <- fluidPage(
    
    theme = shinytheme("cosmo"),

    titlePanel("Shiny Data Explorer"),

    sidebarLayout(
        sidebarPanel(
            fileInput("file", "Choose CSV File", accept = ".csv"),
            actionButton("upload", "Run / Reset", width = "100%"),
            tags$hr(),
            uiOutput("renderColumns")
        ),
        mainPanel(
            actionButton("update", "Init / Refresh Columns", width = "40%"),
            tags$hr(),
            verbatimTextOutput("summary")
        )
    )
)

server <- function(input, output) {
    
    dat <- eventReactive(input$upload, {
        uploadedFile <- input$file
        req(uploadedFile)
        read_csv(uploadedFile$datapath, col_names = TRUE)
    })
    
    colnames <- reactive({ names(dat()) })
    coltypes <- reactive({ map_chr(dat(), class) })
    
    # modify data frame columns to specified types
    df <- eventReactive(input$update, {
        
        dd <- dat()
        
        # form expressions in modifying dataframe,
        # based on input${id}
        exprs <- map_chr(colnames(), ~ {
            .in  <- paste0("input$", .x)
            type <- eval(parse_expr(.in))
            glue("dd$`{.x}` <- as.{type}(dd[['{.x}']])")
        })
        
        # since we're in `purrr`, we have to go back 1 more gen
        map(exprs, ~ eval(parse_expr(.x), envir = parent.frame(2)))
        
        return(dd)
        
    })
    
    output$renderColumns <- renderUI({
        
        # form dynamic ui expressions here 
        exprs <- 
            map2_chr(colnames(), coltypes(), 
                    ~ glue(
                    'selectInput("{.x}", "{.x}", 
                    choices = c("character", "numeric", "Date"), 
                    selected = "{.y}")')
                )
        
        # evaluate expressions
        map(exprs, ~ eval(parse_expr(.x)))
    })
    
    output$summary <- renderPrint({
        skimr::skim_without_charts(df())
    })
}

shinyApp(ui = ui, server = server)
