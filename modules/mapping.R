library(ggplot2)

plot_states <- function(data) {
  ggplot(data, aes(x = reorder(State, Cases), y = Cases)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    coord_flip() +
    theme_minimal() +
    labs(title = "Cases by State")
}