library(tidyr)
library(readr)
library(dplyr)
library(shiny)

# Define UI for data upload app ----
ui <- fluidPage(

  # App title ----
  titlePanel("JFS CAN File Prep"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      width = 2,
      
      # Input: Select a file ----
      fileInput("file", "Choose CSV File",
                multiple = TRUE,
                placeholder = "Browse...",
                buttonLabel = "Import File",
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      downloadButton('download', "Download Report")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      rclipboard::rclipboardSetup(),
      
      fluidRow(
        column(
          h3("Original Data"),
          tableOutput("original"), width = 6),
        column(
          h3("Prepped Data"),
          tableOutput("prepped"), width = 4)
      ),
      verbatimTextOutput("console")
    )
    
  )
)

# Define server logic to read selected file ----
server <- function(input, output, session) {
  
  options(shiny.maxRequestSize = 500 * 1024^2)
  
  output$original <- renderTable({
    
    req(input$file)
    
    df <- read_csv(input$file$datapath)
    
    validate(
      need("ALLEGATION_ADDRESS" %in% names(df),
           "No ALLEGATION_ADRESS column found in the input file")
    )
    validate(
      need("CHILD_ADDRESS" %in% names(df),
           "No CHILD_ADRESS column found in the input file")
      
    )
    
    
    head(df[,c("INTAKE_ID", "ALLEGATION_ADDRESS", "CHILD_ADDRESS")])
    
  })
  
  d_prepped <- reactive({
    
    req(input$file)
    
    d <- read_csv(input$file$datapath)
    
    d <- d |> 
      pivot_longer(cols = c(ALLEGATION_ADDRESS, CHILD_ADDRESS),
                   names_to = 'address_type',
                   values_to = 'address')
    
    d_prepped <- d
  })
  
  output$prepped <- renderTable({
    
    head(d_prepped()[, c("INTAKE_ID",  "address")])
    
  })
  
  output$download <- downloadHandler(
    file <-  "intake_report.html",
    content = function(file) {
      tempReport <- file.path(tempdir(), "intake_report.Rmd")
      file.copy("intake_report.Rmd", tempReport, overwrite = TRUE)
      params <- list(d_prepped = d_prepped())
      rmarkdown::render(tempReport, 
                        output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
  # output$download <-  downloadHandler(
  #       file <-  "intake_report.html",
  #       content = function(file) {
  #         tempReport <- file.path(tempdir(), "intake_report.Rmd")
  #         file.copy("intake_report.Rmd", tempReport, overwrite = TRUE)
  #         params <- list(d_prepped = d_prepped())
  #         
  #         withCallingHandlers({
  #           shinyjs::html("console", "")
  #           
  #           rmarkdown::render(tempReport, 
  #                             output_file = file,
  #                             params = params,
  #                             envir = new.env(parent = globalenv()),
  #                             quiet = FALSE
  #           )
  #           
  #         },
  #         message = function(m) {
  #           shinyjs::html(id = "console", html = m$message, add = TRUE)
  #         })
  #       }
  # )
  
  
}
# Run the app ----
shinyApp(ui, server)