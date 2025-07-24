library(dplyr)
library(purrr)
library(lubridate)
library(slider)
library(shiny)
library(tibble)
library(readr)
library(DT)

telework_file <- "data/telework_tracking.csv"

pay_periods <- read_csv("data/pay_periods.csv") |>
  mutate(start_date = mdy(start_date) |> as_date(), 
         end_date = mdy(end_date) |> as_date())

# Initialize telework hours if file doesn't exist
if (!file.exists(telework_file)) {
  telework <- pay_periods |>
    mutate(telework_hours = 0, 
           telework_hours_2_periods = 0)
  write_csv(telework, telework_file)
}


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
  telework_data <- reactiveVal(read_csv(telework_file))
  
  observeEvent(input$submit, {
    # Load current data
    df <- telework_data()
    
    # Find the row that matches the date
    df <- df |>
      mutate(telework_hours = if_else(
        input$new_date >= start_date & input$new_date <= end_date,
        telework_hours + input$new_hours,
        telework_hours
      ))
    
    # Recalculate the 2-period rolling sum
    df <- df |>
      arrange(start_date) |>
      mutate(telework_hours_2_periods = slide_sum(
        telework_hours, before = 1, after = 1
      ))
    
    # Save and update
    write_csv(df, telework_file)
    telework_data(df)
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

