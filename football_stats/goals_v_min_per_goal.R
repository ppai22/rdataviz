library(rvest)
library(tidyverse)
library(ggrepel)
library(cowplot)


# kURL <- "https://www.transfermarkt.co.in/manchester-united/toptorschuetzen/verein/985/plus/1/galerie/0"
kURL <- "https://www.transfermarkt.co.in/manchester-united/toptorschuetzen/verein/985/plus/1/galerie/0?wettbewerb_id=GB1&position=alle&detailposition=alle"


html.data <- read_html(kURL)

names <- html.data %>% html_nodes('tbody .inline-table .hauptlink .spielprofil_tooltip') %>% html_text()
goals <- html_text(html_nodes(html.data, 'tbody .hauptlink'))[1:2==2] %>% as.numeric()
minutes.per.goal <- html_text(html_nodes(html.data, 'tbody .zentriert'))[1:11==9]
for (i in 1:length(minutes.per.goal)) {
  minutes.per.goal[[i]] <- str_replace(minutes.per.goal[[i]], "'", "")
}
minutes.per.goal <- minutes.per.goal %>% as.numeric()

data <- data.frame(names, goals, minutes.per.goal)
colnames(data) <- c("Name", "Goals", "MinsPerGoal")

man.utd.plot <- ggplot(data, aes(x = goals, y = -minutes.per.goal)) +
  geom_point() +
  geom_text_repel(label = names, max.overlaps = 50) +
  theme(aspect.ratio = 1/1)
man.utd.plot

kURL.prem.league <- "https://www.transfermarkt.co.in/premier-league/torschuetzenliste/wettbewerb/GB1/saison_id//altersklasse/alle/detailpos//plus/1"
html.data.pl <- read_html(kURL.prem.league)
pl.names <- html.data.pl %>% html_nodes('tbody .inline-table .hauptlink .spielprofil_tooltip') %>% html_text()
pl.goals <- html_text(html_nodes(html.data.pl, 'tbody .hauptlink'))[1:2==2] %>% as.numeric()
pl.minutes.per.goal <- html_text(html_nodes(html.data.pl, 'tbody .rechts'))[1:2==2]
for (i in 1:length(pl.minutes.per.goal)) {
  pl.minutes.per.goal[[i]] <- str_replace(pl.minutes.per.goal[[i]], "'", "")
}
pl.minutes.per.goal <- pl.minutes.per.goal %>% as.numeric()

prem.league.data <- data.frame(pl.names, pl.goals, pl.minutes.per.goal)
colnames(prem.league.data) <- c("Name", "Goals", "MinsPerGoal")

prem.league.plot <- ggplot(prem.league.data, aes(x = pl.goals, y = -pl.minutes.per.goal)) +
  geom_point() +
  geom_text_repel(label = pl.names, max.overlaps = 50) +
  theme(aspect.ratio = 1/1)

prem.league.plot

plot_grid(man.utd.plot, prem.league.plot)

