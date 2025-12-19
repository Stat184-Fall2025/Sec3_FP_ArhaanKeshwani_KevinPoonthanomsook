# Code Chunk 1 - Import Libraries and Raw Data
# Written by: Kevin Poonthanomsook, Reviewed by: Arhaan Keshwani
# Using tidyverse style guide for all R code in this project

# Import libraries
library(tidyverse)
library(googlesheets4)
library(rvest)

# Use googlesheets4 library to import pro player in-game stats
gs4_deauth()
sheet_url <- "https://docs.google.com/spreadsheets/d/1XeDEL-1jZ-P4gTezOREV11O4bRiFXlBXRhc1tXSPx1g/edit?gid=139406561#gid=139406561"

# Get all of the tab names from the sheet
tabs <- sheet_names(sheet_url)
all_tabs <- lapply(tabs, function(tab) read_sheet(sheet_url, sheet = tab))
names(all_tabs) <- tabs

# Convert all values in data that are lists to strings
clean_tabs <- lapply(all_tabs, function(df) {
  df %>%
    mutate(across(where(is.list), ~ sapply(., toString)))
})

# Combine all rows across all tabs into one combined dataframe
combined <- bind_rows(clean_tabs, .id = "sheet_name")

# Scrape all table data from top players of rocket league on esportsearnings
earnings_raw <- read_html(
  "https://www.esportsearnings.com/games/409-rocket-league/top-players#google_vignette"
) %>%
  html_element(css = "table") %>%
  html_table()

# Perform an inner join on raw earnings data and raw in-game stats by player
result_data <- inner_join(combined, earnings_raw, by = c("Player" = "Player ID"))


# Code Chunk 2 - Wrangle and Clean Combined Data
# Written by: Arhaan Keshwani, Reviewed by: Kevin Poonthanomsook

# Clean data
result_clean <- result_data %>%
  # Coalesce 2 columns whose values represent the same stat
  mutate(Demo...19 = coalesce(Demo...19, Demos)) %>%
  # Select necessary columns to keep for analysis
  select(Player, GP, Score...14, Goal...15, Assist...16, Save...17, Shot...18,
         Demo...19, `Shot %`, `Total (Overall)`) %>%
  # Rename all columns for tidyness and readability
  setNames(c("Player", "Games_Played", "Avg_Score", "Avg_Goal", 
             "Avg_Assist", "Avg_Save", "Avg_Shot", "Avg_Demo", 
             "Shot_Percentage", "Total_Earnings")) %>%
  # Format earnings to remove dollar sign and be numeric
  mutate(
    Total_Earnings = str_remove_all(Total_Earnings, "[$,]"),
    Total_Earnings = as.numeric(Total_Earnings)
  )


# Code Chunk 3 - Exploratory Data Analysis 1
# Written by: Arhaan Keshwani, Reviewed by: Kevin Poonthanomsook

# Plot a scatterplot of average score vs. total earnings with a best fit line
ggplot(result_clean, aes(x = Avg_Score, y = Total_Earnings)) +
  # Create scatterplot
  geom_point(color = "blue", alpha = 0.7, size = 3) +
  # Apply axis labels and title
  labs(
    title = "Avg Score vs Total Earnings",
    x = "Average Score",
    y = "Total Earnings (USD)"
  ) +
  # Make theme minimal for easier viewing
  theme_minimal() + 
  # Create line of best fit for scatter
  geom_smooth(method = "lm", se = FALSE, color = "red")

ggplot(result_clean, aes(x = Avg_Goal, y = Total_Earnings)) +
  geom_point(color = "blue", alpha = 0.7, size = 3) +
  labs(
    title = "Avg Goals vs Total Earnings",
    x = "Average Goals",
    y = "Total Earnings (USD)"
  ) +
  theme_minimal() + 
  geom_smooth(method = "lm", se = FALSE, color = "red")

ggplot(result_clean, aes(x = Avg_Assist, y = Total_Earnings)) +
  geom_point(color = "blue", alpha = 0.7, size = 3) +
  labs(
    title = "Avg Assists vs Total Earnings",
    x = "Average Assists",
    y = "Total Earnings (USD)"
  ) +
  theme_minimal() + 
  geom_smooth(method = "lm", se = FALSE, color = "red")

ggplot(result_clean, aes(x = Avg_Save, y = Total_Earnings)) +
  geom_point(color = "blue", alpha = 0.7, size = 3) +
  labs(
    title = "Avg Saves vs Total Earnings",
    x = "Average Saves",
    y = "Total Earnings (USD)"
  ) +
  theme_minimal() + 
  geom_smooth(method = "lm", se = FALSE, color = "red")

ggplot(result_clean, aes(x = Avg_Demo, y = Total_Earnings)) +
  geom_point(color = "blue", alpha = 0.7, size = 3) +
  labs(
    title = "Avg Demolitions vs Total Earnings",
    x = "Average Demolitions",
    y = "Total Earnings (USD)"
  ) +
  theme_minimal() + 
  geom_smooth(method = "lm", se = FALSE, color = "red")

ggplot(result_clean, aes(x = Shot_Percentage, y = Total_Earnings)) +
  geom_point(color = "blue", alpha = 0.7, size = 3) +
  labs(
    title = "Shot Percentage vs Total Earnings",
    x = "Shot Percentage",
    y = "Total Earnings (USD)"
  ) +
  theme_minimal() + 
  geom_smooth(method = "lm", se = FALSE, color = "red")
