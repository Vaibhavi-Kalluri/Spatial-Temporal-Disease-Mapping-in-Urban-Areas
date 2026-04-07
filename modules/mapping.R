library(dplyr)

state_analysis <- function(data) {
  
  data <- data %>%
    mutate(DeathRate = ifelse(Cases > 0, (Deaths / Cases) * 100, 0))
  
  state_summary <- data %>%
    group_by(State) %>%
    summarise(
      total_cases = sum(Cases, na.rm = TRUE),
      total_deaths = sum(Deaths, na.rm = TRUE),
      death_rate = mean(DeathRate, na.rm = TRUE),
      .groups = "drop"
    )
  
  top5 <- state_summary %>%
    arrange(desc(death_rate)) %>%
    slice(1:5)
  
  state_summary <- state_summary %>%
    mutate(
      Highlight = ifelse(State %in% top5$State, "Top 5", "Others")
    )
  
  return(state_summary)
}