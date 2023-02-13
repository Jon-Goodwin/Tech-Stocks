### Load Libraries
library(tidyverse)
library(tidytuesdayR)
library(gganimate)
library(gapminder)
### Loading Data
tuesdata <- tidytuesdayR::tt_load('2023-02-07')

### Creating Tables

big_stock_prices <- tuesdata$big_tech_stock_prices
tech_names <- tuesdata$big_tech_companies

### Daily animation

timeline <- big_stock_prices %>%
  filter('2022-12-30' > date)

## Change table

## Renaming
tech_names_short <- c("Apple", "Adobe", "Amazon", "Salesforce", 
                      "Cisco Systems", "Alphabet", "IBM", "Intel", "META", 
                      "Microsoft", "Netflix", "Nvidia", "Oracle", "Tesla")

tech_stock_rename <- tibble(stock_symbol = tech_names$stock_symbol, names = tech_names_short)



timeline_ranked <- timeline %>%
  mutate(date = format(date, "%Y-%m")) %>%
  group_by(stock_symbol, date) %>%
  summarise(adj_close = round(mean(adj_close), 2)) %>%
  ungroup() %>%
  group_by(date) %>%
  mutate(rank = min_rank(-adj_close) * 1.0) %>%
  select(stock_symbol, date, adj_close,rank)

timeline_ranked <- full_join(timeline_ranked, tech_stock_rename, by = c("stock_symbol" = "stock_symbol"))

timeline_ranked %>%
  select(names, date, adj_close, rank)

## Create plots, Need to add better labels and titles.

ranked_day <- ggplot(timeline_ranked, aes(rank, group = names,
                             fill = as.factor(names))) +
  geom_tile(aes(y = adj_close/2, 
                height = adj_close,
                width = 0.9), alpha = 0.9, color = NA) +
  coord_flip(clip = "off", expand = FALSE) +
  geom_text(aes(y = 0, label = paste(names, " ")), vjust = 0.2, hjust = 1,
            fontface = "bold", size = 6) +
  geom_text(aes(y = adj_close, label = str_c("$",round(adj_close,2))), hjust=0,
            size = 5, fontface = "bold") +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_line( size=.1, color="grey" ),
        panel.grid.minor.x = element_line( size=.1, color="grey" ),
        plot.title=element_text(size=25, hjust=0.5, face="bold", colour="grey", vjust=-1),
        plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="grey"),
        plot.caption =element_text(size=8, hjust=0.5, face="italic", color="grey"),
        plot.background=element_blank(),
        plot.margin = margin(2,2, 2, 4, "cm")) +
  transition_states(date, transition_length = 4, state_length = 2) +
  view_follow(fixed_x = TRUE) + 
  labs(title = 'Date : {closest_state}',
       subtitle  =  "Tech Stock Prices")
N = length(unique(timeline_ranked$date))
P = 150

## animate

animation <- animate(ranked_day,
        nframes = N+P, fps = 10, width = 1400, end_pause = P, height = 1000)
