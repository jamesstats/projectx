# Load necessary packages
library(readxl)
library(ggplot2)
library(dplyr)

# Load the spreadsheet
file_path <- "/mnt/data/greatbritain.xlsx"
sheet_name <- "comp"
data <- read_excel("~/Documents/elections_files/greatbritain.xlsx", sheet = 'comp')

# Extract the year from the Party column and remove the year suffix from Party names
data <- data %>%
  mutate(Year = as.numeric(gsub(".*(\\d{2})$", "20\\1", Party)),
         Party = gsub("\\d{2}", "", Party))

# Separate data for 2019 and 2024 based on the Year column
data_2019 <- data %>%
  filter(Year == 2019) %>%
  rename(Polling_2019 = Share)

data_2024 <- data %>%
  filter(Year == 2024) %>%
  rename(Polling_2024 = Share)

# Merge the 2019 and 2024 data on Days and Party
data_combined <- merge(data_2019, data_2024, by = c("Days", "Party"))

# Calculate the difference in polling percentages
data_combined <- data_combined %>%
  mutate(Difference = Polling_2024 - Polling_2019) %>%
  select(Days, Party, Polling_2019, Polling_2024, Difference)

# Create the line chart
ggplot(data_combined, aes(x = Days)) +
  geom_line(aes(y = Polling_2019, color = "2019")) +
  geom_line(aes(y = Polling_2024, color = "2024")) +
  geom_line(aes(y = Difference, color = "Difference")) +
  labs(title = "Polling Difference Between 2019 and 2024",
       x = "Days to Election",
       y = "Polling Percentage",
       color = "Legend") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.title = element_text(size = 12)
  )
