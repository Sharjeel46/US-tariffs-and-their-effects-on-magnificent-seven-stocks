# render_animation.R

library(quantmod)  # Get stock data
library(tidyverse) # Data manipulation & plotting
library(gganimate) # Animation
library(gifski)    # GIF rendering


tickers <- c("AAPL", "MSFT", "AMZN", "GOOGL", "NVDA", "META", "TSLA")
getSymbols(tickers, src = "yahoo", from = "2018-01-01", to = Sys.Date())

# Combine and clean data
prices <- map(tickers, ~Ad(get(.x))) %>% 
  reduce(merge) %>% 
  `colnames<-`(tickers) %>% 
  fortify.zoo(name = "Date") %>% 
  pivot_longer(-Date, names_to = "Stock", values_to = "Price")


events <- tibble(
  Date = as.Date(c("2018-03-22", "2018-07-06", "2018-09-24",
                  "2019-05-10", "2019-09-01", "2025-06-15")),
  Label = c("$50B Tariff", "First Tariff", "$200B Tariffs",
           "25% Increase", "Consumer Tariffs", "New Tariff"),
  y_end = c(800, 850, 900, 950, 1000, 1050) # Line stopping points
)


ggplot(prices, aes(Date, Price, color = Stock)) +
  geom_line(linewidth = 1) +
  
  # Red stopping lines
  geom_segment(
    data = events,
    aes(x = Date, xend = Date, y = 0, yend = y_end),
    color = "red", linetype = "dashed", linewidth = 0.7,
    inherit.aes = FALSE
  ) +
  
  # Event labels
  geom_label(
    data = events,
    aes(Date, y_end * 1.05, label = Label),
    color = "black", fill = "white", size = 3.5,
    inherit.aes = FALSE
  ) +
  
  scale_color_brewer(palette = "Set1") +
  labs(title = "Stock Prices with Tariff Events") +
  theme_minimal() +
  
  transition_reveal(Date)

animate(
  last_plot(),
  fps = 20,
  duration = 35,
  width = 900,
  height = 600,
  renderer = gifski_renderer("stocks_tariffs.gif")
)