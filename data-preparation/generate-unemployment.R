# Load necessary libraries
library(readxl)
library(dplyr)
library(lubridate)

# Path to the Excel file
file_path <- "C:/Users/nrothlin/Downloads/je-d-03.03.02.05.xlsx"

# Get sheet names
sheets <- excel_sheets(file_path)

# Select years 2014 to 2024 sheets
years <- as.character(2014:2024)
selected_sheets <- sheets[sheets %in% years]


# Initialize an empty list to store data
data_list <- list()

# Loop through each sheet and extract relevant data
for (year in selected_sheets) {
  # Read the sheet
  sheet_data <- read_excel(file_path, sheet = year, col_names = FALSE)

  # Find rows containing "VK 2)" or "Total"
  vk_row <- which(apply(sheet_data, 1, function(row) any(row == "VK 2)")))
  total_row <- which(apply(sheet_data, 1, function(row) any(row == "Total")))

  # Debug: Print row indices
  print(paste("Sheet:", year, "VK 2) Row:", vk_row, "Total Row:", total_row))

  # Ensure both rows are found
  if (length(vk_row) > 0 && length(total_row) > 0) {
    # Extract relevant rows and transpose them
    relevant_rows <- sheet_data[c(vk_row, total_row), ] %>%
      t() %>%  # Transpose for tidy format
      as.data.frame(stringsAsFactors = FALSE)

    # Debug: Print extracted rows
    print(paste("Processing sheet:", year))
    print(head(relevant_rows))

    # Name the columns for clarity
    colnames(relevant_rows) <- c("Date", "UnemploymentRate")

    # Directly use the data without modifying dates
    relevant_rows <- relevant_rows %>%
      mutate(UnemploymentRate = as.numeric(UnemploymentRate))  # Convert to numeric

    # Add to list
    data_list[[year]] <- relevant_rows
  } else {
    warning(paste("Rows for 'VK 2)' or 'Total' not found in sheet:", year))
  }
}


# Combine all data into a single data frame
combined_data <- bind_rows(data_list)

combined_data <- combined_data |> filter(!(is.na(Date) & is.na(UnemploymentRate)))
combined_data <- combined_data |> filter(Date != "VK 2)")

# Replace 'Date' with the last date of the month
combined_data <- combined_data %>%
  mutate(
    Date = as.Date(paste0(Date, "-01")),  # Add a day to convert 'YYYY-MM' to 'YYYY-MM-DD'
    Date = ceiling_date(Date, "month") - days(1)  # Replace with the last date of the month
  )


# Save the combined dataset as a CSV (optional)
write.csv2(combined_data, "R:/Masterarbeit/market/combined_unemployment_data.csv", row.names = FALSE)
