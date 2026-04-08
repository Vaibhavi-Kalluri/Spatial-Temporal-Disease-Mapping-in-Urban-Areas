library(tidyverse)

covid <- read.csv("data/covid.csv")
malaria <- read.csv("data/malaria.csv", check.names = FALSE)
dengue <- read.csv("data/dengue.csv", check.names = FALSE)

colnames(covid) <- make.names(colnames(covid))
covid_clean <- covid %>%
  mutate(Date = as.Date(Date)) %>%  
  select(State.UnionTerritory, Date, Confirmed, Deaths, Cured) %>%
  rename(State = State.UnionTerritory) %>%
  mutate(
    Year = format(Date, "%Y"),
    Disease = "COVID"
  ) %>%
  group_by(State, Year, Disease) %>%
  summarise(
    Cases = max(Confirmed, na.rm = TRUE),
    Deaths = max(Deaths, na.rm = TRUE),
    .groups = "drop"
  )

colnames(malaria) <- gsub(" - ", "_", colnames(malaria))
colnames(malaria) <- gsub("\\(.*\\)", "", colnames(malaria))
colnames(malaria) <- gsub("\\s+", "", colnames(malaria))

malaria_long <- malaria %>%
  pivot_longer(
    cols = -c(`Sl.No.`, `State/UT`),
    names_to = c("Year", "Type"),
    names_pattern = "(\\d{4})_(Cases|Deaths)",
    values_to = "Count"
  ) %>%
  rename(State = `State/UT`) %>%
  mutate(Disease = "Malaria")

malaria_clean <- malaria_long %>%
  pivot_wider(names_from = Type, values_from = Count)

colnames(dengue) <- gsub(" - ", "_", colnames(dengue))
colnames(dengue) <- gsub("\\*+", "", colnames(dengue))
colnames(dengue) <- gsub("\\s+", "", colnames(dengue))

dengue_long <- dengue %>%
  pivot_longer(
    cols = -`State/UT`,
    names_to = c("Year", "Type"),
    names_pattern = "(\\d{4})_(Cases|Deaths)",
    values_to = "Count"
  ) %>%
  rename(State = `State/UT`) %>%
  mutate(Disease = "Dengue")

dengue_clean <- dengue_long %>%
  pivot_wider(names_from = Type, values_from = Count)

final_data <- bind_rows(
  covid_clean %>% select(State, Year, Disease, Cases, Deaths),
  malaria_clean %>% select(State, Year, Disease, Cases, Deaths),
  dengue_clean %>% select(State, Year, Disease, Cases, Deaths)
)

cleaned_data <- final_data
library(stringr)

cleaned_data <- cleaned_data %>%
  mutate(
    State = str_trim(State),
    State = str_replace_all(State, "\\*+", ""),
    State = str_replace_all(State, "\\.+", ""),
    State = str_to_title(State)
  )
cleaned_data <- cleaned_data %>%
  mutate(
    State = recode(State,
                   "Pudducherry" = "Puducherry",
                   "Laddakh" = "Ladakh"
    )
  )

cleaned_data <- cleaned_data %>%
  filter(State != "Total")

cleaned_data <- cleaned_data %>%
  mutate(
    State = case_when(
      State %in% c("Dadra And Nagar Haveli", "Daman And Diu") ~ 
        "Dadra And Nagar Haveli And Daman And Diu",
      TRUE ~ State
    )
  )
cleaned_data <- cleaned_data %>%
  filter(
    !State %in% c(
      "Total",
      "Cases Being Reassigned To States",
      "Cases Being Reassigned To State",
      "Unknown",
      "Others",
      "Unassigned"
    )
  )

head(final_data)
str(final_data)
summary(final_data)
