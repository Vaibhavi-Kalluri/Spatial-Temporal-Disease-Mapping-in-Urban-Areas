library(ggplot2)

plot_cases <- function(data) {
  ggplot(data, aes(x = Year, y = Cases, color = Disease)) +
    geom_line(size = 1) +
    theme_minimal() +
    labs(title = "Cases Trend")
}

plot_deaths <- function(data) {
  ggplot(data, aes(x = Year, y = Deaths, color = Disease)) +
    geom_line(size = 1) +
    theme_minimal() +
    labs(title = "Deaths Trend")
}