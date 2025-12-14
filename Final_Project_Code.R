library(tidyverse)
library(googlesheets4)
library(rvest)

gs4_deauth()
sheet_url <- "https://docs.google.com/spreadsheets/d/1XeDEL-1jZ-P4gTezOREV11O4bRiFXlBXRhc1tXSPx1g/edit?gid=139406561#gid=139406561"

tabs <- sheet_names(sheet_url)
all_tabs <- lapply(tabs, function(tab) read_sheet(sheet_url, sheet = tab))
names(all_tabs) <- tabs

clean_tabs <- lapply(all_tabs, function(df) {
  df %>%
    mutate(across(where(is.list), ~ sapply(., toString)))
})

combined <- bind_rows(clean_tabs, .id = "sheet_name")
View(combined)

earnings_raw <- read_html(
  "https://www.esportsearnings.com/games/409-rocket-league/top-players#google_vignette"
) %>%
  html_element(css = "table") %>%
  html_table()
View(earnings_raw)

result <- inner_join(combined, earnings_raw, by = c("Player" = "Player ID"))
View(result)
colnames(result)

result_clean <- result %>%
  mutate(Demo...19 = coalesce(Demo...19, Demos)) %>%
  select(Player, GP, Score...14, Goal...15, Assist...16, Save...17, Shot...18, Demo...19, `Shot %`, `Total (Overall)`) %>%
  setNames(c("Player", "Games_Played", "Avg_Score", "Avg_Goal", "Avg_Assist", 
             "Avg_Save", "Avg_Shot", "Avg_Demo", "Shot_Percentage", "Total_Earnings")) %>%
  mutate(
    Total_Earnings = str_remove_all(Total_Earnings, "[$,]"),
    Total_Earnings = as.numeric(Total_Earnings)
  )
View(result_clean)

ggplot(result_clean, aes(x = Avg_Score, y = Total_Earnings)) +
  geom_point(color = "blue", alpha = 0.7, size = 3) +
  labs(
    title = "Avg Score vs Total Earnings",
    x = "Average Score",
    y = "Total Earnings (USD)"
  ) +
  theme_minimal() + 
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
