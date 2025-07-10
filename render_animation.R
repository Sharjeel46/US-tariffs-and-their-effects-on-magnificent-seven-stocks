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
# Stock prices with evets 

# List of stock ticker symbols (Magnificent Seven)
tickers <- c("AAPL", "MSFT", "AMZN", "GOOGL", "NVDA", "META", "TSLA")

# Download Adjusted Closing Prices from Yahoo Finance (only for 2018–2019)
getSymbols(tickers, src = "yahoo", from = "2018-01-01", to = "2019-12-31")

# Convert price data to tidy format (Date, Stock, Price)
prices <- map(tickers, ~ Ad(get(.x))) %>%
  reduce(merge) %>%                       # Merge all stock time series
  `colnames<-`(tickers) %>%              # Rename columns with tickers
  fortify.zoo(name = "Date") %>%         # Convert to data frame with Date column
  pivot_longer(-Date, names_to = "Stock", values_to = "Price")  # Long format

#  Defines tariff events BEFORE trying to mutate 
events <- tibble(
  Date = as.Date(c("2018-03-22", "2018-07-06", "2018-09-24",
                   "2019-05-10", "2019-09-01")),
  Label = c("$50B Tariff", "First Tariff", "$200B Tariffs",
            "25% Increase", "Consumer Tariffs"),
  y_end = c(800, 850, 900, 950, 1000)
)
# Red shaded windows represent key tariff periods (e.g., 5 days around announcement)
tariff_windows <- tibble(
  start = as.Date(c("2018-07-01", "2018-09-20", "2019-05-05")),
  end   = as.Date(c("2018-07-10", "2018-09-30", "2019-05-15")),
  event = c("First Tariff", "$200B Tariffs", "25% Increase")
)



#  all available market dates from the stock data
available_dates <- unique(prices$Date)

# Replace each event date with the nearest available trading day
events <- events %>%
  mutate(Date = if_else(Date %in% available_dates, Date,
                        map_dbl(Date, ~ max(available_dates[available_dates <= .x])) %>%
                          as.Date(origin = "1970-01-01")))


# Joined prices and events to create inline labels like: "$200B Tariffs\n$176.53"
inline_event_labels <- inner_join(prices, events, by = "Date") %>%
  mutate(Label = paste0(Label, "\n", dollar(Price)))

# Creates the animated line plot
p <- ggplot(prices, aes(x = Date, y = Price, color = Stock, group = Stock)) +
  
  # Line chart of stock prices
  geom_line(linewidth = 1) +
  
  # Red transparent shaded zones during tariff windows
  geom_rect(
    data = tariff_windows,
    aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
    fill = "red", alpha = 0.12,
    inherit.aes = FALSE
  ) +
  
  # Red dashed vertical lines for key event dates
  geom_segment(
    data = events,
    aes(x = Date, xend = Date, y = 0, yend = y_end),
    color = "red", linetype = "dashed", linewidth = 0.7,
    inherit.aes = FALSE
  ) +
  
  # White banners with tariff labels at the top
  geom_label(
    data = events,
    aes(x = Date, y = y_end * 1.05, label = Label),
    fill = "white", color = "black", size = 3.5,
    inherit.aes = FALSE
  ) +
  
  # Floating dollar value label at each point in time
  geom_text(
    aes(label = dollar(Price)),
    size = 2.5, vjust = -1, check_overlap = TRUE,
    show.legend = FALSE
  ) +
  
  # Yellow inline labels showing price at the tariff events
  geom_label(
    data = inline_event_labels,
    aes(x = Date, y = Price, label = Label),
    fill = "yellow", color = "black", fontface = "bold",
    size = 3.3, label.size = 0.3, vjust = -1,
    inherit.aes = FALSE
  ) +
  
  # Styling
  scale_y_continuous(labels = dollar_format()) +
  scale_color_brewer(palette = "Set1") +
  labs(
    title = "Trump  Fitst term Tariff Events and Their Impact on Stock Prices",
    subtitle = "Date: {frame_along}",
    x = "Date", y = "Stock Price (USD)"
  ) +
  theme_minimal(base_size = 12) +
  
  # Animate the chart over time
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
