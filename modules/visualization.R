library(ggplot2)
library(plotly)

plot_cases <- function(data) {
  p <- ggplot(data, aes(x = Year, y = Cases, color = Disease)) +
    geom_line(size = 1) +
    theme_minimal() +
    labs(title = "Cases Trend")
  
  ggplotly(p)
}

plot_deaths <- function(data) {
  p <- ggplot(data, aes(x = Year, y = Deaths, color = Disease)) +
    geom_line(size = 1) +
    theme_minimal() +
    labs(title = "Deaths Trend")
  
  ggplotly(p)
}