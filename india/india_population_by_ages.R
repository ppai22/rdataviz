library(tidyverse)
library(httr)


# Read data from Our World In Data
kURL <- "https://raw.githubusercontent.com/owid/owid-datasets/master/datasets/Population%20by%20age%20group%20to%202100%20(based%20on%20UNWPP%2C%202017%20medium%20scenario)/Population%20by%20age%20group%20to%202100%20(based%20on%20UNWPP%2C%202017%20medium%20scenario).csv"
resp <- GET(kURL)
raw.data <- read.csv(kURL)
# Filter out India data
india.data <- raw.data %>% filter(Entity == "India")
# Rename columns
colnames(india.data) <- c("Entity", "Year", "below15", 
                          "below65above15", "over65", 
                          "below5", "below14above5", 
                          "below24above15", "below65above25")
# Fetch data that we want and convert to long format for stacked chart
plot.data <- india.data %>% select(Year, below15, below65above15, over65) %>% gather(class, value, below15:over65)
# Get current year
current.year <- as.numeric(format(Sys.Date(), "%Y"))
# Plot data
ggplot(data = plot.data, aes(x = Year, y = value, fill = class)) +
  # Stacked area chart with percentage instead of absolute values
  geom_area(position = position_fill(reverse = TRUE)) +
  # Modifying labels and colours for the categories
  scale_fill_manual(labels = c("0-14 years", "15-65", "65+"), values = c("#e68609", "#337eb0", "#821717")) +
  # Adding a vertical dashed line for current year
  geom_vline(xintercept = current.year, colour = "white", linetype = "dashed", size = 1) +
  # Labelling the vline
  geom_text(aes(x = current.year + 0.5, y = 0.5, label = "Current year"), angle = 90, colour = "white") +
  # Formatting the Y-axis ticks
  scale_y_continuous(name = NULL, labels = function(x) paste(as.character(x*100), "%", sep = "")) +
  # Adding x ticks for every ten years
  scale_x_continuous(n.breaks = 10) +
  # Adding title, sub-title and footer
  labs(
    title = "Breakdown of India's population by age group",
    subtitle = "Proportion of total population (1950-2050)",
    caption = "Source: Our World In Data\nPrepared by: @ppai22\nInspiration: https://www.bbc.com/news/world-asia-china-46772503"
  ) +
  # Making the plot compact
  coord_cartesian(xlim = c(1960, 2050), ylim = c(0, 1), expand = FALSE) +
  theme_light() +
  theme(
    aspect.ratio = 9/16,
    # Removing legend title, positioning it to the top and aligning to the left of the image
    legend.title = element_blank(),
    legend.position = "top",
    legend.justification = "left",
    # Adding colour to axes and ticks
    axis.line = element_line(colour = "black"),
    axis.ticks = element_line(colour = "black"),
    # Adding doem margin around the plot
    plot.margin = unit(c(1,1,1,1), "cm"),
    # Aligning the footer to the left
    plot.caption = element_text(face = "italic", hjust = 0)
  ) +
  ggsave("india_population_by_ages.png", width = 16, height = 9)

