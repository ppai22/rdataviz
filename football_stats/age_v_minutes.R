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
  scale_x_continuous(name = "Age", n.breaks = 25) +
  labs(
    title = "Manchester United",
    subtitle = "Comparison of Ages and Minutes Played for 2020-21",
    caption = "Data from https://www.transfermarkt.co.in/\nPrepared by @ppai22"
    ) +
  theme_light() +
  theme(
    plot.background = element_rect(fill = "#800000"),
    panel.background = element_rect(fill = "#800000"),
    panel.border = element_rect(colour = "black"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linetype = "dashed", colour = "black"),
    axis.line = element_line(colour = "black"),
    axis.text = element_text(color = "black"),
    axis.ticks = element_line(colour = "black"),
    plot.title = element_text(colour = "black", hjust = 0.5, size = 20),
    plot.subtitle = element_text(colour = "black", hjust = 0.5, size = 15),
    plot.caption = element_text(face = "italic")
  )
ggsave("age_v_minutes.png", width = 15, height = 10)
