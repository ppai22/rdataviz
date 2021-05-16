library(tidyverse)
library(cowplot)


# Loading the data and cleaning the date_added format
netflix_titles <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-20/netflix_titles.csv') %>% 
  mutate(date_added = as.Date(date_added, "%B %d, %Y"))
# Adding new column year_added to the data
netflix_titles$year_added <- as.numeric(format(netflix_titles$date_added, "%Y"))

# Filtering data by TV Show and Movie
tv.shows <- netflix_titles %>% filter(type == "TV Show")
movies <- netflix_titles %>% filter(type == "Movie")

# Custom theme
theme_custom <- function() {
  return(theme_light() +
           theme(
             plot.title = element_text(colour = "red", hjust = 0.5),
             plot.background = element_rect(fill = "black", colour = "black"),
             plot.margin = unit(c(1, 1, 1, 1), "cm"),
             panel.grid = element_blank(),
             panel.background = element_rect(fill = "black"),
             panel.border = element_rect(linetype = "dashed"),
             axis.text = element_text(colour = "grey85"),
             axis.title = element_text(colour = "grey85"),
             legend.background = element_rect(fill = "black"),
             legend.text = element_text(colour = "grey85")
             )
         )
}

# Plots for number of TV Shows and Movies released by year
tv.plot <- ggplot(data = tv.shows) +
  geom_histogram(aes(x = release_year), binwidth = 1, colour = "red", fill = "red") +
  scale_x_continuous(name = "Year of Release") +
  scale_y_continuous(name = "Count") +
  labs(title = "TV Shows released over the years") +
  theme_light() +
  theme_custom()
movies.plot <- ggplot(data = movies) +
  geom_histogram(aes(x = release_year), binwidth = 1, colour = "red", fill = "red") +
  scale_x_continuous(name = "Year of Release") +
  scale_y_continuous(name = "Count") +
  labs(title = "Movies released over the years") +
  theme_light() +
  theme_custom()

# Plots of TV Shows and Movies added by date mapped with their release years
tv.show.date.added <- ggplot(data = tv.shows) +
  geom_point(aes(y = release_year, x = date_added), colour = "red") +
  scale_x_date(name = "Date Added") +
  scale_y_continuous(name = "Year of Release") +
  labs(title = "TV Shows added to Netflix by year") +
  theme_light() +
  theme_custom()
movies.date.added <- ggplot(data = movies) +
  geom_point(aes(y = release_year, x = date_added), colour = "red") +
  scale_x_date(name = "Date Added") +
  scale_y_continuous(name = "Year of Release") +
  labs(title = "Movies added to Netflix by year") +
  theme_light() +
  theme_custom()

# Plots for number of TV Shows and Movies added by year and the same data displayed as percentage
stacked.chart.abs <- ggplot(netflix_titles, aes(x = year_added, fill = type)) +
  geom_bar() +
  labs(title = "Number of TV shows and Movies added by year") +
  scale_fill_manual(values = c("red", "grey85")) +
  scale_x_continuous(name = "Year Added", n.breaks = 14) +
  scale_y_continuous(name = "Count") +
  coord_flip() +
  theme_light() +
  theme_custom() +
  theme(legend.title = element_blank(), legend.position = "top")
stacked.chart.percentage <- ggplot(netflix_titles, aes(x = year_added, fill = type)) +
  geom_bar(position = "fill") +
  labs(title = "Number of TV shows and Movies added by percentage by year") +
  scale_fill_manual(values = c("red", "grey85")) +
  scale_x_continuous(name = "Year Added", n.breaks = 14) +
  scale_y_continuous(name = "Percentage", labels = function(x) x*100) +
  coord_flip() +
  theme_light() +
  theme_custom() +
  theme(legend.title = element_blank(), legend.position = "top")

# Combining all the plots
plot_grid(
  tv.plot, tv.show.date.added, stacked.chart.abs, 
  movies.plot, movies.date.added, stacked.chart.percentage,
  ncol = 3
  ) +
  labs(
    title = "NETFLIX TITLES",
    caption = "Source: Kaggle\nTidy Tuesday Challenge\nPrepared by: @ppai22"
  ) +
  theme_cowplot() +
  theme(
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    plot.background = element_rect(fill = "black"),
    plot.title = element_text(colour = "red", hjust = 0.5, size = 25),
    plot.caption = element_text(colour = "grey85", face = "italic", hjust = 1)
  ) +
  ggsave("netflix_titles_data.png", height = 10, width = 15)
