# ztime
Telework tracker
# 📊 Telework Tracker Shiny App

This Shiny app allows users to submit and track telework requests. It displays two reactive tables:
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

## NOTE: As of 30 July 2025, this app only covers pay periods for CY 2025. Multi-year functionality is planned for future updates. 
