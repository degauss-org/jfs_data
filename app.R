library(tidyr)
library(readr)
library(dplyr)
library(shiny)

# Define UI for data upload app ----
ui <- fluidPage(

  # App title ----
  titlePanel("AFT File Ingestion"),
  
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
      need("Allegation Address" %in% names(df),
           "No Allegation Addresss column found in the input file")
    )
    validate(
      need("Child's Address" %in% names(df),
           "No Child's Address column found in the input file")
      
    )
    
    
    head(df[,c("Intake ID", "Allegation Address", "Child's Address")])
    
  })
  
  d_prepped <- reactive({
    
    req(input$file)
    
    d <- read_csv(input$file$datapath)
    
    d <- d |> 
      rename_with(toupper, everything()) |> 
      rename_with(~stringr::str_replace_all(.x, " ", "_"), everything()) |> 
      rename(CHILD_ADDRESS = "CHILD'S_ADDRESS",
             CHILD_ADDRESS_START = "CHILD'S_ADDRESS_START",
             FAM_ASSES_DECISION = "FAM_ASSESS._DECISION") |> 
      pivot_longer(cols = c(ALLEGATION_ADDRESS, CHILD_ADDRESS),
                   names_to = 'address_type',
                   values_to = 'address')
    
    d_prepped <- d
  })
  
  output$prepped <- renderTable({
    
    head(d_prepped()[, c("INTAKE_ID",  "address")])
    
  })
  
  output$download <- downloadHandler(
    filename = function() {
      "aft_intake_data_prepped.csv"
      },
    content = function(file) {
      write_csv(d_prepped(), file)
    }
  )
  
}
# Run the app ----
shinyApp(ui, server)