library(tidyr)
library(readr)
library(dplyr)
library(shiny)
library(addr)
library(shinycssloaders)

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
      
      uiOutput('pin_button')
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      fluidRow(
        column(
          h3("Original Data"),
          withSpinner(tableOutput("original")), width = 6),
        column(
          h3("Prepped Data"),
          withSpinner(tableOutput("prepped")), width = 4)
      ),
      fluidRow(
        column(tableOutput("summary_table_1"), width = 5),
        column(tableOutput("summary_table_2"), width = 5)
        )
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
  
  d_geocoded <- reactive({
    
    req(input$file)

    d <- read_csv(input$file$datapath)
    d <- read_csv('test_sacwis_output.csv')
    
    d <- d |> 
      rename_with(toupper, everything()) |> 
      rename_with(~stringr::str_replace_all(.x, " ", "_"), everything()) |> 
      rename(CHILD_ADDRESS = "CHILD'S_ADDRESS",
             CHILD_ADDRESS_START = "CHILD'S_ADDRESS_START",
             FAM_ASSES_DECISION = "FAM_ASSESS._DECISION",
             MANDATED_REPORTER = "MANDATED_REPORTER?") 
    
    #just keep one record for each person-decision point; don't need record of initial decision and final decision
    d <- d |> 
      group_by(DECISION_DATE, PERSON_ID) |> 
      slice_tail() |> 
      ungroup()
    
    num_unique_person_dates <- nrow(d)
    
    #if two different addresses, keep allegation; if one address, keep whichever it is
    d1 <- d |> 
      filter(ALLEGATION_ADDRESS == "Unknown Address" | is.na(ALLEGATION_ADDRESS)) |> 
      mutate(address = CHILD_ADDRESS)
    
    used_child_address <- nrow(d1)

    d2 <- d |>
      filter(!INTAKE_ID %in% d1$INTAKE_ID) |> 
      mutate(address = ALLEGATION_ADDRESS)
    
    
     d3 <- rbind(d1, d2) |> 
       select(-c(ALLEGATION_ADDRESS, CHILD_ADDRESS))
     
     no_address_recorded <- nrow(
       d3 |> 
         filter(is.na(address)) 
     )
    
    t <- d3 |> 
      tidyr::drop_na(address) |> 
      mutate(clean_address = toupper(clean_address_text(address))) 
    
    t$addr <- as_addr(t$clean_address)
    
    num_addr_attempted_match <- nrow(t)
    
    t$cagis_addr_matches <- addr_match(t$addr, cagis_addr$cagis_addr)
    
    t2 <- t |>
      mutate(
        addr_match_result =
          case_when(
            purrr::map_lgl(cagis_addr_matches, is.null) ~ NA,
            purrr::map_dbl(cagis_addr_matches, vctrs::vec_size) == 0 ~ "no_match",
            purrr::map_dbl(cagis_addr_matches, vctrs::vec_size) == 1 ~ "single_match",
            purrr::map_dbl(cagis_addr_matches, vctrs::vec_size) > 1 ~ "multi_match",
            .default = "foofy"
          ) |>
          factor(levels = c("no_match", "single_match", "multi_match"))
      )
    
    print_addr_matches <- t2 |> 
      select(addr_match_result) |> 
      group_by(addr_match_result) |> 
      summarise(n_results = n()) |> 
      ungroup()

    
    output$summary_table_1 <- renderTable({
      req(input$file)
      
       tibble::enframe(c(num_unique_person_dates = num_unique_person_dates, 
                         used_child_address = used_child_address, 
                         no_address_recorded = no_address_recorded,
                         num_addr_attempted_match = num_addr_attempted_match)) 
    })
    
    output$summary_table_2 <- renderTable({  
      print_addr_matches 
    })
    
    t3 <- t2  |>
      filter(addr_match_result %in% c("single_match")) |> 
      #tibble::enframe(name = "input_addr", value = "cagis_addr") |>
      dplyr::mutate(cagis_addr = purrr::list_c(cagis_addr_matches)) |>
      dplyr::left_join(cagis_addr, by = "cagis_addr")
    
    t4 <- t3 |> 
      tidyr::unnest(cagis_addr_data) |> 
      tidyr::drop_na(cagis_s2) |> 
      mutate(block_group_id = tiger_block_groups(s2::as_s2_cell(cagis_s2)), year = '2010') 
    
    t5 <- t4|> 
      mutate(census_tract_id = stringr::str_sub(block_group_id, end = -2)) 
    
    d_geocoded <- t5 |> 
      select(INTAKE_ID, SCREENING_DECISION, DECISION_DATE, BIRTH_DATE, PERSON_ID, RACE,
             MANDATED_REPORTER, block_group_id, census_tract_id) 

  })
  
  output$prepped <- renderTable({
    
    
    head(d_geocoded()[, c("INTAKE_ID", "census_tract_id")])
    
  })
  
  output$pin_button <- renderUI({
    req(d_geocoded())
    
    actionButton('pin', "Save data")
  })
  
  observeEvent(input$pin, {
    
    salt_board <- pins::board_connect(auth = "manual",
                                      server = Sys.getenv("CONNECT_SERVER_DEV"),
                                      key = Sys.getenv("CONNECT_API_KEY_DEV"))
    
    salt_board |> 
      pins::pin_write(d_geocoded(), "AFT_intake_data_geocoded")
    
    showModal(
      modalDialog(
        title = "Data successfully saved!",
        "You may now exit this application",
        easyClose = TRUE,
        footer = NULL
    ))
    
  })
  
  
}
# Run the app ----
shinyApp(ui, server)