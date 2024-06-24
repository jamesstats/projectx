library(ggplot2)
library(dplyr)

# Sample data
data <- data.frame(
  date = rep(seq(as.Date("2019-01-01"), as.Date("2019-12-31"), by="month"), 2),
  party = rep(c("Conservative", "Labour"), each=12),
  polling = c(40, 35, 42, 37, 44, 38, 46, 39, 48, 40, 50, 42, 
              45, 38, 47, 39, 49, 41, 51, 42, 53, 43, 55, 44),
  year = rep(c("2019", "2024"), each=12)
)

ggplot(data, aes(x = date, y = polling, color = party)) +
  geom_line(aes(linetype = year)) +
  geom_point(aes(shape = year)) +
  labs(title = "UK General Election Polling: 2019 vs 2024", 
       x = "Date", 
       y = "Polling Percentage",
       color = "Party",
       linetype = "Year",
       shape = "Year") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.title = element_text(size = 12)
  )
ggplot(data, aes(x = date, y = polling, color = party)) +
  geom_line(aes(linetype = year), size = 1) +
  geom_point(aes(shape = year), size = 3) +
  labs(title = "UK General Election Polling: 2019 vs 2024", 
       x = "Date", 
       y = "Polling Percentage",
       color = "Party",
       linetype = "Year",
       shape = "Year") +
  scale_color_manual(values = c("Conservative" = "blue", "Labour" = "red")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.title = element_text(size = 12),
    legend.position = "bottom"
  )

