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

# Plotts for number of TV Shows and Movies released by year
tv.plot <- ggplot(data = tv.shows) +
  geom_histogram(aes(x = release_year), binwidth = 1)
movies.plot <- ggplot(data = movies) +
  geom_histogram(aes(x = release_year), binwidth = 1)

# Plots of TV Shows and Movies added by date mapped with their release years
tv.show.date.added <- ggplot(data = tv.shows) +
  geom_point(aes(y = release_year, x = date_added))
movies.date.added <- ggplot(data = movies) +
  geom_point(aes(y = release_year, x = date_added))

# Plots for number of TV Shows and Movies added by year and the same data displayed as percentage
stacked.chart.abs <- ggplot(netflix_titles, aes(x = year_added, fill = type)) +
  geom_bar() +
  coord_flip() +
  theme(legend.position = "top")
stacked.chart.percentage <- ggplot(netflix_titles, aes(x = year_added, fill = type)) +
  geom_bar(position = "fill") +
  coord_flip() +
  theme(legend.position = "top")

# Combining all the plots
plot_grid(
  tv.plot, tv.show.date.added, stacked.chart.abs, 
  movies.plot, movies.date.added, stacked.chart.percentage,
  ncol = 3
  ) +
  ggsave("netflix_titles_data.png", height = 10, width = 15)
