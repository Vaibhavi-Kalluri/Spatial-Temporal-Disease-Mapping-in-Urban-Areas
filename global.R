library(shiny)
library(tidyverse)
library(ggplot2)
library(plotly)
library(lubridate)

source("modules/data_cleaning.R")

# Convert Year to numeric
cleaned_data$Year <- as.numeric(cleaned_data$Year)

# Remove NA values
cleaned_data <- cleaned_data %>%
  filter(!is.na(Cases), !is.na(Deaths))