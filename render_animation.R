# render_animation.R

library(quantmod)     # For fetching financial data
library(tidyverse)    # For data manipulation and ggplot
library(gganimate)    # For creating animations
library(gifski)       # For rendering GIFs
library(scales)       # For formatting prices as dollars


#| echo: false
#| message: false
#| warning: false
#| results: 'hide'

# the Magnificent 7 tickers
tickers <- c("AAPL", "MSFT", "AMZN", "GOOGL", "NVDA", "META", "TSLA")

# Adjusted Closing Prices from Yahoo Finance
getSymbols(tickers, src = "yahoo", from = "2018-01-01", to = Sys.Date())

# Combined and reshaped the data to tidy format data
prices <- map(tickers, ~ Ad(get(.x))) %>%
  reduce(merge) %>%
  `colnames<-`(tickers) %>%
  fortify.zoo(name = "Date") %>%
  pivot_longer(-Date, names_to = "Stock", values_to = "Price")

# Define all tariffs event dates and their labels
events <- tibble(
  Date = as.Date(c("2018-03-22", "2018-07-06", "2018-09-24",
                   "2019-05-10", "2019-09-01", "2025-06-15")),
  Label = c("$50B Tariff", "First Tariff", "$200B Tariffs",
            "25% Increase", "Consumer Tariffs", "New Tariff"),
  y_end = c(800, 850, 900, 950, 1000, 1050)
)

# all event dates match actual stock price trading days
available_dates <- unique(prices$Date)

# Replaced missing event dates with closest available dates
events <- events %>%
  mutate(Date = if_else(Date %in% available_dates, Date,
                        map_dbl(Date, ~ max(available_dates[available_dates <= .x])) %>% as.Date(origin = "1970-01-01")))

# Joind each event date with each stock to annotate on the lines
inline_event_labels <- inner_join(prices, events, by = "Date") %>%
  mutate(Label = paste0(Label, "\n", dollar(Price)))

#  the animated plot
p <- ggplot(prices, aes(x = Date, y = Price, color = Stock, group = Stock)) +

  # Priceing trend lines
  geom_line(linewidth = 1) +

  # Red dashed vertical lines for events
  geom_segment(
    data = events,
    aes(x = Date, xend = Date, y = 0, yend = y_end),
    color = "red", linetype = "dashed", linewidth = 0.7,
    inherit.aes = FALSE
  ) +

  # Top-of-chart event name banners
  geom_label(
    data = events,
    aes(x = Date, y = y_end * 1.05, label = Label),
    fill = "white", color = "black", size = 3.5,
    inherit.aes = FALSE
  ) +

  # Floating price labels at each frame
  geom_text(
    aes(label = dollar(Price)),
    size = 2.5, vjust = -1, check_overlap = TRUE,
    show.legend = FALSE
  ) +

  #  boxes for all events per stock on their lines
  geom_label(
    data = inline_event_labels,
    aes(x = Date, y = Price, label = Label),
    fill = "yellow", color = "black", fontface = "bold",
    size = 3.3, label.size = 0.3, vjust = -1,
    inherit.aes = FALSE
  ) +

  # Scales and styling
  scale_y_continuous(labels = dollar_format()) +
  scale_color_brewer(palette = "Set1") +
  labs(
    title = "Stock Prices with U.S. Tariff Events",
    subtitle = "Date: {frame_along}",
    x = "Date", y = "Price (USD)"
  ) +
  theme_minimal(base_size = 12) +

  # Animated along time
  transition_reveal(Date)

# Animated and render for 40 seconds
animate( 
  last_plot,
  p,
  duration = 40,              # total animation time
  fps = 20,                   # frames per second
  width = 900,
  height = 600,
  renderer = gifski_renderer("stocks_tariffs.gif")
)
