## app.R ##
library(shiny)
library(shinydashboard)
library(tidytuesdayR)
library(tidyverse)
library(scales)
library(lubridate)

tuesdata <- tidytuesdayR::tt_load('2023-02-07')

stock_prices <- tuesdata$big_tech_stock_prices
companies <- tuesdata$big_tech_companies

fouryears <- lubridate::make_date(seq(year(min(stock_prices$date)),
                                      year(max(stock_prices$date)),
                                      by = 4
), 1, 1)

ui <- dashboardPage(
  dashboardHeader(title = "Big Tech Stocks"),
  
  dashboardSidebar(
    selectInput("v_company", "Company", choices = stock_prices %>%
                  select(stock_symbol) %>% 
                  distinct() %>% 
                  arrange(stock_symbol) %>% 
                  drop_na())
  ),
  dashboardBody(
    fluidRow(box(plotOutput("daily_volume")), box(plotOutput("highest_volume"))),
    fluidRow(box(plotOutput("daily_price")), box(plotOutput("highest_price")))
  )
)

server <- function(input, output) {
  
  output$daily_volume <- renderPlot({
    
    stock_prices %>%
      filter(stock_symbol == input$v_company) %>%
      select(date, volume) %>%
      ggplot(aes(x = date, y = volume)) +
      geom_line() +
      scale_x_date(NULL,
                   breaks = fouryears, date_labels = "%Y"
      ) +
      ylab(NULL) +
      scale_y_continuous(labels = label_comma()) +
      labs(title = "Daily Trading Volume")
  })
  
  output$highest_volume <- renderPlot({
    
    stock_prices %>%
      filter(stock_symbol == input$v_company) %>%
      select(volume,date) %>%
      top_n(volume, n= 5) %>%
      mutate(date = fct_reorder(as.factor(date), -volume)) %>%
      ggplot(aes(x = date, y = volume, fill = volume)) +
      geom_col() +
      scale_y_continuous(labels = label_comma()) +
      xlab(NULL) +
      ylab(NULL) +
      theme(legend.position = "none", 
            axis.text.y = element_text(color = "grey20", size = 12),
            axis.text.x = element_text(color = "grey20", size = 12)) +
      labs(title = "Highest Volume Days")
  })
  
  output$highest_price <- renderPlot({
    
    stock_prices %>%
      filter(stock_symbol == input$v_company) %>%
      select(adj_close, open,date) %>%
      mutate(per_change = ((adj_close - open)/open)) %>%
      select(date, per_change) %>%
      top_n(per_change, n= 5) %>%
      mutate(date = fct_reorder(as.factor(date), -per_change)) %>%
      ggplot(aes(x = date, y = per_change, fill = per_change)) +
      geom_col() + 
      scale_y_continuous(labels = scales::percent) +
      xlab(NULL) +
      ylab(NULL) +
      theme(legend.position = "none", 
            axis.text.y = element_text(color = "grey20", size = 12),
            axis.text.x = element_text(color = "grey20", size = 12)) +
      labs(title = "Largest Positive Days", 
           subtitle = "Difference between open and closing price")
  })
  
  
  output$daily_price <- renderPlot({
    
    stock_prices %>%
      filter(stock_symbol == input$v_company) %>%
      select(date, adj_close) %>%
      ggplot(aes(x = date, y = adj_close)) +
      geom_line() +
      ylab(NULL) +
      scale_x_date(NULL,
                   breaks = fouryears, date_labels = "%Y"
      ) +
      scale_y_continuous(labels = label_comma()) +
      labs(title = "Daily Stock Price")
    
  })
  
  
}


shinyApp(ui, server)