library(rvest)
library(tidyverse)


CategorizeRestDays <- function(days) {
  # Method to categorize rest days
  if (days >=5) {
    return("> 5 days")
  }
  else if (days >= 3 & days < 5) {
    return("3 - 4 days")
  }
  else if (days >= 1 & days < 3) {
    return("1 - 2 days")
  }
}

# URL for data
kURL <- "https://www.transfermarkt.co.in/manchester-united/spielplan/verein/985/saison_id/2020"

html.data <- read_html(kURL)
# Various tables in the data fetches as separate data frames
prem.league.data <- data.frame(html_table(html_nodes(html.data, xpath = '//*[@id="main"]/div[10]/div[1]/div[7]/div[3]/table')))
europa.league.data <- data.frame(html_table(html_nodes(html.data, xpath = '//*[@id="main"]/div[10]/div[1]/div[6]/div[3]/table')))
champions.league.data <- data.frame(html_table(html_nodes(html.data, xpath = '//*[@id="main"]/div[10]/div[1]/div[5]/div[3]/table')))
fa.cup.data <- data.frame(html_table(html_nodes(html.data, xpath = '//*[@id="main"]/div[10]/div[1]/div[8]/div[3]/table')))
efl.cup.data <- data.frame(html_table(html_nodes(html.data, xpath = '//*[@id="main"]/div[10]/div[1]/div[9]/div[3]/table')))
# Data is combined to one data frame, dates are cleaned up for format and Opponents names are cleaned up
full.data <- rbind(prem.league.data, europa.league.data, champions.league.data, fa.cup.data, efl.cup.data) %>% 
  mutate(Date = as.Date(Date, "%a %b %d, %Y"), Opponent = str_replace(paste(str_trim(Opponent.1, side = "both"), " "), "\\([0-9]+\\.\\)", ""))
# Matches are ordered based on date
full.data <- full.data[order(full.data$Date),]
# Number of rest days between games are calculated
no.of.days.rest <- c()
for (i in 2:length(full.data$Date)) {
  no.of.days.rest[i] <- CategorizeRestDays(as.numeric(full.data$Date[[i]] - full.data$Date[[i-1]]) - 1)
}
no.of.days.rest[[1]] <- "Beginning of season"
# Adding column with Rest Days class for each game
full.data$RestDays <- no.of.days.rest
# Resetting the data frame row order
row.names(full.data) <- NULL

# Plotting the grap
ggplot(full.data, aes(x = Date, y = "Matches")) +
  # Colours are based on the rest days classification
  geom_point(aes(color = factor(RestDays)), size = 3) +
  geom_line() +
  scale_color_manual(values = c("Green", "Red", "Orange", "Black")) +
  # Adding xtiks for each week
  scale_x_date(name = NULL, date_breaks = "weeks", date_labels = "%d %b %y") +
  # Formatting Y-axis
  scale_y_discrete(name = NULL) +
  # Labelling each point with the Opponent name rotated by 90 degrees
  geom_text(aes(label = Opponent), angle = 90, hjust = 1, size = 3.5) +
  labs(
    title = "Fixture Congestion (2020-21) - Manchester United",
    caption = "Source: https://www.transfermarkt.co.in/\nPrepared by: @ppai22"
  ) +
  theme_light() +
  theme(
    panel.grid = element_blank(),
    legend.position = "top",
    legend.justification = "left",
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5),
    plot.caption = element_text(face = "italic", hjust = 0)
    ) +
  ggsave("fixture_congestion.png", width = 15, height = 4)

