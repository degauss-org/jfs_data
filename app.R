library(tidyr)
library(readr)
library(dplyr)
library(shiny)
library(stringr)


# Define UI for data upload app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("JFS CAN File Prep and Commands"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      width = 2,
      
      # Input: Select a file ----
      fileInput("file", "Choose CSV File",
                multiple = TRUE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      downloadButton('download', "Download prepped file")
      
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
      fluidRow(
        verbatimTextOutput("newname")
      ),
      h3("Docker Commands"),
      fluidRow(
        splitLayout(
          cellWidths = c("1.5%", "85%", "10%"),
          p(strong("1. ")),
          verbatimTextOutput("command1"),
          uiOutput("clip1")
        )
      ),
      
      hr(style = "border-top: 1px solid #FFF;"),
      
      fluidRow(
        splitLayout(
          cellWidths = c("1.5%", "85%", "10%"),
          p(strong("2. ")),
          verbatimTextOutput("command2"),
          uiOutput("clip2")
        )
      ),
      
      hr(style = "border-top: 1px solid #FFF;"),
      
      fluidRow(
        splitLayout(
          cellWidths = c("1.5%", "85%", "10%"),
          p(strong("3. ")),
          verbatimTextOutput("command3"),
          uiOutput("clip3")
        )
      )
    )
    
  )
)

# Define server logic to read selected file ----
server <- function(input, output, session) {
  
  output$original <- renderTable({
    
    req(input$file)
    
    df <- read_csv(input$file$datapath)
    
    
    head(df[,c("INTAKE_ID", "ALLEGATION_ADDRESS", "CHILD_ADDRESS")])
    
  })
  
  d_prepped <- reactive({
    
    req(input$file)
    
    d <- read_csv(input$file$datapath)
    
    d <- d |> 
      pivot_longer(cols = c(ALLEGATION_ADDRESS, CHILD_ADDRESS),
                   names_to = 'address_type',
                   values_to = 'address')
    
    d <- d |> 
      mutate(address = str_replace_all(address, '[[:blank:]]', ' '))
    
    d <- d |> 
      mutate(address = str_replace_all(address, fixed('\\'), ''),
             address = str_replace_all(address, fixed('"'), ''),
             address = str_replace_all(address, '[^[:alnum:] ]', ''))
    
    foster_char_strings <- c('Ronald McDonald House',
                             '350 Erkenbrecher Ave',
                             '350 Erkenbrecher Avenue',
                             '350 Erkenbrecher Av',
                             '222 East Central Parkway',
                             '222 East Central Pkwy',
                             '222 East Central Pky',
                             '222 Central Parkway',
                             '222 Central Pkwy',
                             '222 Central Pky',
                             '3333 Burnet Ave',
                             '3333 Burnet Avenue',
                             '3333 Burnet Av',
                             'verify',
                             'foreign',
                             'foreign country',
                             'unknown')
    d <- d |> 
      mutate(bad_address = purrr::map(address, ~ str_detect(.x, coll(foster_char_strings, ignore_case=TRUE)))) %>%
      mutate(bad_address = purrr::map_lgl(bad_address, any))
    
    d[is.na(d$address), 'bad_address'] <- TRUE
    
    no_no_regex_strings <- c('(10722\\sWYS)',
                             '\\bP(OST)*\\.*\\s*[O|0](FFICE)*\\.*\\sB[O|0]X',
                             '(3333\\s*BURNETT*\\s*A.*452[12]9)')
    d <- d |> 
      mutate(PO = purrr::map(address, ~ str_detect(.x, regex(no_no_regex_strings, ignore_case=TRUE)))) %>%
      mutate(PO = purrr::map_lgl(PO, any))
    
    d_prepped <- d |> 
      filter(!bad_address & !PO)
    
    d_prepped
  })
  
  output$prepped <- renderTable({
    
    head(d_prepped()[, c("INTAKE_ID",  "address")])
    
  })
  
  output$newname <- renderText({
    
    req(input$file)
    
    name <- str_sub(input$file$name, end = -5)
    name_prepped <- paste0(name,"_prepped")
    
    glue::glue("New input file name: {name_prepped}.csv")
  })
  
  
  output$command1 <- renderPrint({
    
    req(input$file)
    
    name <- str_sub(input$file$name, end = -5)
    name_prepped <- paste0(name,"_prepped")
    
    cmd1 <- glue::glue("docker run --rm -v ${{pwd}}:/tmp ghcr.io/degauss-org/geocoder {name_prepped}.csv")
    
    print(cmd1)
    
    output$clip1 <- renderUI({
      req(input$file)
      
      rclipboard::rclipButton(
        inputId = "clip1",
        label = "Copy",
        clipText = cmd1,
        icon = icon("clipboard")
      )
    })

  })
  
  
  
  output$command2 <- renderPrint({
    
    req(input$file)
    
    name <- str_sub(input$file$name, end = -5)
    name_prepped <- paste0(name,"_prepped")
    
    cmd2 <- glue::glue("docker run --rm -v ${{pwd}}:/tmp ghcr.io/degauss-org/census_block_group {name_prepped}_geocoder_3.3.0_score_threshold_0.5.csv")
    
    print(cmd2)
    
    output$clip2 <- renderUI({
      req(input$file)
      
      rclipboard::rclipButton(
        inputId = "clip2",
        label = "Copy",
        clipText = cmd2,
        icon = icon("clipboard")
      )
    })
  })
  
  
  output$command3 <- renderPrint({
    
    req(input$file)
    
    name <- str_sub(input$file$name, end = -5)
    name_prepped <- paste0(name,"_prepped")
    
    cmd3 <- glue::glue("docker run --rm -v ${{pwd}}:/tmp degauss/jfs_aggregated_data_report:5.0.0 {name_prepped}_geocoder_3.3.0_score_threshold_0.5_census_block_group_0.6.0_2010.csv")
    
    print(cmd3)
    
    output$clip3 <- renderUI({
      req(input$file)
      
      rclipboard::rclipButton(
        inputId = "clip3",
        label = "Copy",
        clipText = cmd3,
        icon = icon("clipboard")
      )
    })
    
  })
  
  output$download <- downloadHandler(
    filename = function() {
      paste(str_sub(input$file$name, end = -5), "_prepped", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(d_prepped(), file, row.names = FALSE)
    }
  )
}
# Run the app ----
shinyApp(ui, server)