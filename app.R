library(tidyr)
library(readr)
library(dplyr)
library(shiny)
library(addr)

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
      
      uiOutput('pin_button'),
      downloadButton('download', "download")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      fluidRow(
        column(
          h3("Original Data"),
          tableOutput("original"), width = 6),
        column(
          h3("Geocoded Data"),
          tableOutput("prepped"), width = 4)
      ),
      hr(),
      fluidRow(
        h3('Summary Tables'),
        column(tableOutput("summary_table_1"), width = 5),
        column(tableOutput("summary_table_2"), width = 5)
        ),
      fluidRow(
        p(strong("Checks:"),"num_addr_attempted_match + no_address_recorded = num_unique_person_dates"),
        p("sum of `addr_match_result` table results should equal num_addr_attempted_match")
      ),
      hr(),
      fluidRow(
        h3("Neighborhood Report"),
        tableOutput("report_table")
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
    
  }, digits = 0)
  
  d_geocoded <- reactive({
    
    req(input$file)

    d <- read_csv(input$file$datapath)
    
    shinybusy::show_modal_progress_circle()
    
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
    
    shinybusy::update_modal_progress(.2)
    
    t$addr <- as_addr(t$clean_address)
    
    
    shinybusy::update_modal_progress(.4)
    
    num_addr_attempted_match <- nrow(t)

    
    t$cagis_addr_matches <- addr_match(t$addr, cagis_addr()$cagis_addr)
    
    shinybusy::update_modal_progress(.8)
    
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
      #tibble::enframe(x = t2$cagis_addr_matches, name = "input_addr", value = "cagis_addr") |>
      dplyr::mutate(cagis_addr = purrr::list_c(list(cagis_addr_matches))) |>
      dplyr::left_join(cagis_addr(), by = "cagis_addr")
    
    shinybusy::update_modal_progress(.9)
    
    t4 <- t3 |> 
      tidyr::unnest(cagis_addr_data) |> 
      tidyr::drop_na(cagis_s2) |> 
      mutate(block_group_id = s2_join_tiger_bg(s2::as_s2_cell(cagis_s2), year = '2013')) 
    
    t5 <- t4|> 
      mutate(census_tract_id = stringr::str_sub(block_group_id, end = -2)) 
    
    d_geocoded <- t5 |> 
      select(INTAKE_ID, SCREENING_DECISION, DECISION_DATE, BIRTH_DATE, PERSON_ID, RACE,
             MANDATED_REPORTER, block_group_id, census_tract_id) 
    

    shinybusy::update_modal_progress(1)

    shinybusy::remove_modal_progress()
    
    d_geocoded

  })
  
  output$prepped <- renderTable({
    
    req(d_geocoded())
    
    head(d_geocoded()[, c("INTAKE_ID", "census_tract_id")])
    
  }, digits = 0)
  
  d_report <- reactive({
    req(d_geocoded())
    
    tract_to_neighborhood <- readRDS('tract_to_neighborhood.rds')
    
    r <- d_geocoded()
    
    r <- r |>
      mutate(screened_in = SCREENING_DECISION %in% c("SCREENED IN", "SCREENED IN AR"))
    
    ##
    r <- r |> 
      slice_head(by = INTAKE_ID)
    
    r <- r  |> 
    #  filter(!duplicated(INTAKE_ID)) |>
      mutate(DECISION_DATE = lubridate::as_date(DECISION_DATE, format = "%m/%d/%Y"),
             BIRTH_DATE = lubridate::as_date(BIRTH_DATE, format = "%m/%d/%Y")) |> 
      mutate(month = lubridate::month(DECISION_DATE),
             year = lubridate::year(DECISION_DATE)) 
    
    #get age, sort into groups
    r <- r |>
      mutate(BIRTH_DATE = replace_na(BIRTH_DATE, as.Date('1900-01-01'))) |>
      mutate(age = as.numeric(difftime(DECISION_DATE, BIRTH_DATE, units = "weeks")/52.25)) |>
      mutate(age_grp = case_when(age <= 5.0 ~ "zero_five",
                                 age < 19 ~ "five_plus",
                                 age >= 19 ~ "no_birth_date"))
    d_neigh <- r  |>
      mutate(fips_tract_id = as.character(census_tract_id)) |> #comment these out when testing with concentrated data
      left_join(tract_to_neighborhood, by='fips_tract_id') |>
      filter(!is.na(DECISION_DATE))
    
    screen_neighborhood_age <- d_neigh |>
      group_by(neighborhood, year, month, age_grp) |>
      summarise(n_screened_in = sum(screened_in == TRUE, na.rm = TRUE),
                n_calls = n(),
                .groups = "keep"
      ) |>
      ungroup() 
    
    screen_neighborhood_MR <- d_neigh |>
      group_by(neighborhood, year, month, MANDATED_REPORTER) |>
      summarise(n_screened_in = sum(screened_in == TRUE, na.rm = TRUE),
                n_calls = n(),
                .groups = "keep"
      ) |>
      ungroup() 
    
    screen_neighborhood_age_report <- screen_neighborhood_age |> 
      group_by(year, month, age_grp) |>
      summarise(neighborhood = "Total",
                n_calls = sum(n_calls),
                n_screened_in = sum(n_screened_in)) %>%
      bind_rows(screen_neighborhood_age, .) |>
      rename(group = "age_grp")
    
    screen_neighborhood_MR_report <- screen_neighborhood_MR |> 
      group_by(year, month, MANDATED_REPORTER) |>
      summarise(neighborhood = "Total",
                n_calls = sum(n_calls),
                n_screened_in = sum(n_screened_in)) %>%
      bind_rows(screen_neighborhood_MR, .)
    
    screen_neighborhood_MR_report <- screen_neighborhood_MR_report |>
      mutate(MANDATED_REPORTER = recode_factor(MANDATED_REPORTER, Yes = "MR", No = "Non_MR")) |>
      rename(group = "MANDATED_REPORTER")
    
    
    screen_neighborhood <- rbind(screen_neighborhood_age_report, screen_neighborhood_MR_report) |>
      arrange(neighborhood, year, month)
    
    screen_neighborhood_2 <- screen_neighborhood |>
      group_by(neighborhood, year, month) |>
      summarise(group = "Total",
                n_calls = sum(n_calls),
                n_screened_in = sum(n_screened_in)) %>%
      bind_rows(screen_neighborhood, .) |>
      arrange(neighborhood, year, month)
    
    screen_neighborhood_rate <- screen_neighborhood_2 |>
      rowwise() |>
      mutate(screen_in_rate = round(n_screened_in / n_calls, 2)) 
    
    d_report <- screen_neighborhood_rate |> 
      drop_na(neighborhood)
    
  })
  
  output$report_table <- renderTable({
    head(d_report() |> filter(neighborhood == "Avondale") |> select(-screen_in_rate))
  }, digits = 0)
  
  output$pin_button <- renderUI({
    req(d_report())
    
    actionButton('pin', "Save data")
  })
  
  observeEvent(input$pin, {
    
    salt_board <- pins::board_connect(auth = "manual",
                                      server = Sys.getenv("CONNECT_SERVER"),
                                      key = Sys.getenv("CONNECT_API_KEY"))

    salt_board |>
      pins::pin_write(d_report(), "AFT_intake_neigbhorhood_report")
    
    
    showModal(
      modalDialog(
        title = "Data successfully saved!",
        "You may now exit this application",
        easyClose = TRUE,
        footer = NULL
    ))
    
  })
  
  output$download <- downloadHandler(
    filename = function() {
      paste("AFT_intake_report_", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(d_report(), file)
    }
  )
  
  
}
# Run the app ----
shinyApp(ui, server)