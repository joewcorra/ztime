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

telework_file <- "data/telework_log.csv"

pay_periods <- read_csv("data/pay_periods.csv") |>
  mutate(pay_period = as_factor(pay_period), 
    start_date = mdy(start_date) |> as_date(), 
         end_date = mdy(end_date) |> as_date())

# Initialize telework request log if file doesn't exist
if (!file.exists(telework_file)) {
  telework_log <- tibble(req_date = c("2025-01-22", "2025-01-23"), 
                         req_hours = c(3, 4))
  write_csv(telework_log, telework_file)
}

telework_log <- read_csv(telework_file)

# Load current data and get pay periods with a fuzzy join
telework_log <- pay_periods |>
  fuzzy_left_join(telework_log, 
                  by = c("start_date" = "req_date", 
                         "end_date" = "req_date"), 
                  match_fun = list(`<=`, `>=`))

# Sum the requested hours by pay period
df <- telework_log |>
  group_by(pay_period, start_date, end_date) |>
  summarize(pp_telework_hours = sum(req_hours)) |>
  ungroup() |>
  arrange(start_date) |>
  mutate(telework_hours_2_periods = slide_sum(
    pp_telework_hours, before = 1, after = 1, na_rm = TRUE
  ))
  

# Define UI
ui <- fluidPage(
  titlePanel("Telework Tracker"),
  sidebarLayout(
    sidebarPanel(
      dateInput("new_date", "Telework Date:"),
      numericInput("new_hours", "Hours:", value = 1, min = 1, max = 24),
      actionButton("submit", "Submit")
    ),
    mainPanel(
      DTOutput("table")
    )
  )
)

# Define Server
server <- function(input, output, session) {
  
  # Reactive value to hold telework data
  telework_data <- reactiveVal(df)
  
  observeEvent(input$submit, {
    
    # Retain only telework rows with date/hr requests
    telework_log <- telework_log |>
      filter(!is.na(req_date))
    
    # Find the row that matches the date
    df <- df |>
      mutate(telework_hours = if_else(
        input$new_date >= start_date & input$new_date <= end_date,
        pp_telework_hours + input$new_hours,
        pp_telework_hours
      ))
    
    # Recalculate the 2-period rolling sum
    df <- df |>
      arrange(start_date) |>
      mutate(telework_hours_2_periods = slide_sum(
        pp_telework_hours, before = 1, after = 1
      ))
    
    # Save and update
    write_csv(telework_log, telework_file)
    
  })
  
  output$table <- renderDT({
    datatable(
      telework_data(),
      options = list(pageLength = 10),
      rownames = FALSE
    ) |>
      formatStyle(
        "telework_hours_2_periods",
        backgroundColor = styleInterval(24, c("white", "tomato"))
      )
  })
}

shinyApp(ui, server)

