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

attempt1

test <- timeline %>%
  mutate(year_month = format(date, '%Y-%m')) %>%
  group_by(stock_symbol,year_month) %>%
  summarise(avg_close = mean(adj_close)) %>%
  ungroup() %>%
  group_by(year_month) %>%
  mutate(rank = min_rank(-avg_close))
  

attempt1 <- ggplot(test, aes(rank, group = stock_symbol,
                             fill = as.factor(stock_symbol))) +
  geom_tile(aes(y = avg_close/2, 
                 height = avg_close,
                 width = 0.9), alpha = 0.9, color = NA) +
  coord_flip(clip = "off", expand = FALSE) +
  geom_text(aes(y = 0, label = paste(stock_symbol, " ")), vjust = 0.2, hjust = 1) +
  geom_text(aes(y = avg_close, label = str_c("$",round(avg_close,2))), hjust=0) +
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
  transition_states(year_month, transition_length = 4, state_length = 2) +
  view_follow(fixed_x = TRUE) + 
  labs(title = 'Date : {closest_state}',
       subtitle  =  "Tech Stock Prices",
       caption  = "Stock Price")

animate(attempt1,renderer = gifski_renderer(),
        nframes = 300, fps = 5,end_pause = 20, width = 1200, height = 1000)

### Daily

timeline <- big_stock_prices %>%
  filter('2022-12-30' > date)

## Change table

timeline_ranked <- timeline %>%
  group_by(date) %>%
  mutate(rank = min_rank(-adj_close) * 1.0) %>%
  ungroup() %>%
  select(stock_symbol, date, adj_close,rank)

## create plots

ranked_day <- ggplot(timeline_ranked, aes(rank, group = stock_symbol,
                             fill = as.factor(stock_symbol))) +
  geom_tile(aes(y = adj_close/2, 
                height = adj_close,
                width = 0.9), alpha = 0.9, color = NA) +
  coord_flip(clip = "off", expand = FALSE) +
  geom_text(aes(y = 0, label = paste(stock_symbol, " ")), vjust = 0.2, hjust = 1) +
  geom_text(aes(y = adj_close, label = str_c("$",round(adj_close,2))), hjust=0) +
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
       subtitle  =  "Tech Stock Prices",
       caption  = "Average Close Price For Month")
N = length(unique(timeline_ranked$date))
P = 150

## animate

animation <- animate(ranked_day,
        nframes = N+P, fps = 15, width = 1200, end_pause = p, height = 1000)
