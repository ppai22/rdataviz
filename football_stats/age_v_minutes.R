library(rvest)
library(tidyverse)
library(ggrepel)


# URLs for Manchester  (For other teams, URLs need to be passed here)
kURL <- "https://www.transfermarkt.co.in/manchester-united/leistungsdaten/verein/985/reldata/%262020/plus/1"
kURL.total.time <- "https://www.transfermarkt.co.in/manchester-united/spielplan/verein/985"

# Scraping data from URLs
html.data <- read_html(kURL)
# Player names
names <- html_text(html_nodes(html.data, 'tbody .posrela .hauptlink .hide-for-small'))
# Minutes played, cleanup, converting to numeric
minutes <- html_text(html_nodes(html.data, 'tbody .rechts  ')) %>% 
  str_remove("'") %>% str_remove("\\.") %>% str_replace("-", "0") %>% as.numeric()
# Player age
ages <- html_text(html_nodes(html.data, 'tbody .zentriert'))[1:13==2] %>% as.numeric()
# Creating the dataframe
data <- data.frame(names, ages, minutes)
# Fetching total matches played by team using second URL
total.matches <- length(html_text(html_nodes(read_html(kURL.total.time), 'tbody tr .zentriert .ergebnis-link')))
# Plotting the data
ggplot(data %>% filter(data$minutes != 0), aes(x = ages, y = minutes)) +
  geom_point(color = "black") +
  # Adding text repel to not have names overlapping
  geom_text_repel(aes(label = names)) +
  # Adding second axis for perecntage of teams minutes played
  scale_y_continuous(name = "Minutes", sec.axis = sec_axis(~./(total.matches * 90) * 100, name = "Percentage of Total minutes")) +
  scale_x_continuous(n.breaks = 25) +
  labs(
    title = "Comparison of age and minutes played (2020-21) - Manchester United",
    caption = "Data from https://www.transfermarkt.co.in/"
    ) +
  theme_light() +
  ggsave("age_v_minutes.png", width = 15, height = 10)
