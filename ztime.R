library(dplyr)
library(purrr)
library(lubridate)
library(slider)
library(shiny)
library(tibble)
library(readr)
library(DT)
library(fuzzyjoin)
library(forcats)
library(tidyr)


telework_file <- "data/telework_log.csv"

pay_periods <- read_csv("data/pay_periods.csv", 
                        col_types = cols(
  pay_period = col_character(),
  start_date = col_character(),
  end_date = col_character())) |>
  mutate(pay_period = as_factor(pay_period), 
    start_date = mdy(start_date) |> as_date(), 
         end_date = mdy(end_date) |> as_date())

# Initialize telework request log if file doesn't exist
if (!file.exists(telework_file)) {
  telework_log <- tibble(req_date = c("2025-01-22", "2025-01-23"), 
                         req_hours = c(0, 0), 
                         row_id = c(0, 0))
  write_csv(telework_log, telework_file)
}

telework_log <- read_csv(telework_file, 
                         col_types = cols(
                           req_date = col_character(),
                           req_hours = col_integer())) |>
  mutate(req_date = ymd(req_date) |> as_date())

# Load current data and get pay periods with a fuzzy join
telework_requests <- pay_periods |>
  fuzzy_left_join(telework_log,
                  by = c("start_date" = "req_date", 
                         "end_date" = "req_date"), 
                  match_fun = list(`<=`, `>=`))

# Sum the requested hours by pay period
df <- telework_requests |>
  group_by(pay_period, start_date, end_date) |>
  summarize(pp_telework_hours = sum(req_hours)) |>
  ungroup() |>
  arrange(start_date) |>
  mutate(pp_telework_hours = replace_na(pp_telework_hours, 0), 
    telework_hours_2_periods = slide_sum(
    pp_telework_hours, before = 1, after = 1, na_rm = TRUE
  ))
  
log <- telework_log

# Define UI----------------------------------------------------
ui <- fluidPage( 
  titlePanel("Telework Tracker"),
  tabsetPanel(
    tabPanel("Submit Request",
             sidebarLayout(
               sidebarPanel(
                 dateInput("new_date", "Telework Date:"),
                 numericInput("new_hours", "Hours:", value = 1, min = 1, max = 12),
                 actionButton("submit", "Submit"),
                 actionButton("save_data", "Save Results to CSV")
               ),
               mainPanel(DTOutput("table"))
             )
    ),
    tabPanel("Telework Log",
                      mainPanel(DTOutput("log_table"),
                                actionButton("delete", "Delete Requests")
             )
    )
  )
)


# Define Server
server <- function(input, output, session) {
  
  # Reactive value to hold telework data
  telework_data <- reactiveVal(df)
  telework_log <- reactiveVal(log)


  observeEvent(input$submit, { 
    # Add to telework_log
    new_log <- telework_log() |>
      add_row(req_date = input$new_date,
              req_hours = input$new_hours)
    
    telework_log(new_log)
    
    # Update telework_data from the full log
    updated_requests <- pay_periods |>
      fuzzy_left_join(new_log,
                      by = c("start_date" = "req_date",
                             "end_date" = "req_date"),
                      match_fun = list(`<=`, `>=`))
    
    df <- updated_requests |>
      group_by(pay_period, start_date, end_date) |>
      summarize(pp_telework_hours = sum(req_hours), .groups = "drop") |>
      arrange(start_date) |>
      mutate(
        pp_telework_hours = replace_na(pp_telework_hours, 0),
        telework_hours_2_periods = slide_sum(pp_telework_hours, before = 1, after = 1, na_rm = TRUE)
      )
    
    telework_data(df)
  })
  

  
  
  output$table <- renderDT({
    # df <- telework_data()
    datatable(
      telework_data(),
      options = list(pageLength = 50),
      rownames = FALSE
    ) |>
      formatStyle(
        "telework_hours_2_periods",
        backgroundColor = styleInterval(24, c("white", "tomato"))
      )
  })
  

  
  observeEvent(input$save_data, {
    write_csv(telework_log() |> 
                filter(req_hours > 0) |> 
                select(req_date, req_hours),
              telework_file)
    
    showNotification("Telework requests saved!", type = "message")
  })
  
  output$log_table <- renderDT({

    # log2 <- telework_log()
   datatable(telework_log(), 
             selection = 'multiple',  # Allow multiple row selection
             options = list(dom = 't') # Only show the table (no search, pagination, etc.)
   )
   
  })
  
  observeEvent(input$delete, {
    selected_rows <- input$log_table_rows_selected
    if (length(selected_rows) > 0) {
      new_log <- telework_log()[-selected_rows, ]
      telework_log(new_log)
      
      # Also update telework_data()
      updated_requests <- pay_periods |>
        fuzzy_left_join(new_log,
                        by = c("start_date" = "req_date",
                               "end_date" = "req_date"),
                        match_fun = list(`<=`, `>=`))
      
      df <- updated_requests |>
        group_by(pay_period, start_date, end_date) |>
        summarize(pp_telework_hours = sum(req_hours), .groups = "drop") |>
        arrange(start_date) |>
        mutate(
          pp_telework_hours = replace_na(pp_telework_hours, 0),
          telework_hours_2_periods = slide_sum(pp_telework_hours, before = 1, after = 1, na_rm = TRUE)
        )
      
      telework_data(df)
    }
  })
  
}


shinyApp(ui, server)

