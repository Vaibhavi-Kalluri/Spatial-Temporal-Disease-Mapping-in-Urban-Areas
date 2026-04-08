predict_future <- function(data) {
  
  data$Year <- as.numeric(data$Year)
  
  model <- lm(Cases ~ Year, data = data)
  
  future <- data.frame(
    Year = seq(max(data$Year), max(data$Year) + 5)
  )
  
  future$Predicted_Cases <- predict(model, future)

  future$Predicted_Cases <- ifelse(
    future$Predicted_Cases < 0,
    0,
    round(future$Predicted_Cases, 2)
  )
  
  return(future)
}