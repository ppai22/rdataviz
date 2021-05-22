library(tidyverse)
library(rvest)
library(ggrepel)


kURL <- "https://stats.espncricinfo.com/ci/content/records/83548.html"

raw.data <- read_html(kURL)
data <- data.frame(html_table(html_nodes(raw.data, 'tbody')))
colnames(data) <- c("Player", "Span", "Matches", "Innings",
                    "NotOuts", "Runs", "HighestScore", "Average",
                    "BallsFaced", "StrikeRate", "Hundreds", "Fifties", "Zeroes", "Fours", "Sixes")
data <- data %>% 
  mutate(Player = str_replace(Player, " \\(.*\\)", ""),
         Fours = as.numeric(str_replace(Fours, "\\+", "")),
         Sixes = as.numeric(str_replace(Sixes, "\\+", "")),
         PercentageRunsFours = round(Fours * 4 / Runs * 100, 2),
         PercentageRunsSixes = round(Sixes * 6 / Runs * 100, 2),
         PercentageRunsBoundaries = PercentageRunsFours + PercentageRunsSixes,
         InningsPerHundred = round(Innings / Hundreds, 1))
ggplot(data, aes(x = Average, y = StrikeRate, size = Runs)) +
  geom_point(color = "green", alpha = 0.5, show.legend = FALSE) +
  geom_text_repel(data = data, aes(label = Player), color = "white", size = 4, max.overlaps = 10) +
  scale_size(range = c(2, 10)) +
  labs(
    title = "Top Run Scorers in One Day Internationals",
    subtitle = "Analysis of Averages vs Strike Rates (Size of bubbles indicates runs scored)",
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
  ggsave("odi_batting_avg_v_st_rate.png", width = 10, height = 10)


