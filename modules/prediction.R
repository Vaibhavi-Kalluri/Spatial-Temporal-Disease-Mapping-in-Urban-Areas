predict_future <- function(data) {
  
  data$Year <- as.numeric(data$Year)
  
  model <- lm(Cases ~ Year, data = data)
  
  future <- data.frame(
    Year = seq(max(data$Year), max(data$Year) + 5)
  )
  
  future$Predicted_Cases <- predict(model, future)
  
  return(future)
}