library(tidyverse)
library(rvest)
library(ggrepel)


kURL <- "https://stats.espncricinfo.com/ci/content/records/283193.html"

raw.data <- read_html(kURL)
data <- data.frame(html_table(html_nodes(raw.data, 'tbody')))
colnames(data) <- c("Player", "Span", "Matches", "Innings",
                    "Balls", "Runs", "Wickets", "BestBowlingInning",
                    "Average", "EconomyRate", "StrikeRate", "FourWicketsInnings", "FiveWicketsInnings")
data <- data %>% 
  mutate(Player = str_replace(Player, " \\(.*\\)", ""))
ggplot(data, aes(x = Average, y = StrikeRate, size = Wickets)) +
  geom_point(color = "yellow", alpha = 0.5, show.legend = FALSE) +
  scale_y_reverse() +
  scale_x_reverse() +
  geom_text_repel(data = data, aes(label = Player), color = "white", size = 4, max.overlaps = 10) +
  scale_size(range = c(2, 10)) +
  labs(
    title = "Top Wicket Takers in One Day Internationals",
    subtitle = "Analysis of Averages vs Strike Rates (Size of bubbles indicates Wickets taken)",
    caption = "Source: ESPNCricinfo\nPrepared by: @ppai22"
  ) +
  theme_light() +
  theme(
    aspect.ratio = 1/1,
    plot.background = element_rect(fill = "black"),
    panel.background = element_rect(fill = "black"),
    panel.grid = element_line(linetype = "dashed", colour = "grey"),
    axis.title = element_text(color = "white"),
    axis.ticks = element_line(colour = "white"),
    axis.text = element_text(colour = "white"),
    plot.title = element_text(colour = "white"),
    plot.subtitle = element_text(colour = "white"),
    plot.caption = element_text(colour = "white", face = "italic"),
    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
  ) +
  ggsave("odi_bowling_avg_v_st_rate.png", width = 10, height = 10)


