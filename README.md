# ztime
Telework tracker for U.S. Environmental Protection Agency civil service employees
# 📊 Telework Tracker Shiny App

This Shiny app allows users to submit and track situational telework requests in accordance with the EPA's Telework and Remote Work Policy issued 24 February 2025, which restricts situational telework to 24 hours over two consecutive pay periods. Requested hours are recorded and tallied over consecutive pay periods; they are flagged in red if any exceed 24 hours over two consecutive pay periods. Results may be saved, if desired, in a CSV called telework_log.csv, which is automatically loaded at the start of each session. If this file does not exist, it is automatically created when the app is run. 
This app displays two reactive tables:
- **Telework Summary Table:** Shows telework hours by pay period and rolling 2-period totals.
- **Telework Log Table:** Lists individual telework requests. Users can delete entries here.

## 🛠 Features
- Submit telework requests by date and number of hours
- Automatically updates pay period totals
- Delete past requests from the log
- Save current log to CSV

---

## 🚀 How to Run the App

### Option 1: Run Locally in RStudio
1. **Clone or download this repository:**
   ```bash
   git clone https://github.com/your-org/telework-tracker.git

install.packages(c(
  "shiny", "dplyr", "purrr", "lubridate", "slider", "tibble",
  "readr", "DT", "fuzzyjoin", "forcats", "tidyr"
))

shiny::runApp()

**NOTE: As of 30 July 2025, this app only covers pay periods for CY 2025. Multi-year functionality is planned for future updates.**

Please notify me of any problems you experience while using this Shiny app. I also welcome any suggestions for improvements. 
