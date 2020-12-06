# Data Science at TUHH ------------------------------------------------------
# Challenge 4.1 ----

# 1.0 Load libraries ----

library(tidyverse)
library(scales)

# 2.0 Import ----

covid_data_tbl <- read_csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv")

# 3.0 Data wrangling ----
covid_tbl <- covid_data_tbl %>%
  
  # Select relevant columns
  select(dateRep, cases, countriesAndTerritories) %>%
  
  # Filter countries, add formatted date, gorup and arrange
  filter(countriesAndTerritories == "Germany" |
           countriesAndTerritories == "Spain" |
           countriesAndTerritories == "France" |
           countriesAndTerritories == "United_Kingdom" |
           countriesAndTerritories == "United_States_of_America") %>%
  mutate(date       = lubridate::dmy(dateRep)) %>% 
  group_by(countriesAndTerritories) %>%
  arrange(date, .by_group = TRUE) %>%
  
  # Add cumulative sum of cases
  mutate(cumsum = cumsum(cases))

# 4.0 Visualization ----

covid_tbl %>%
  
  # Canvas
  ggplot(aes(date, cumsum, color = Country)) +
  
  geom_line(size = 0.7, aes(color = countriesAndTerritories)) +
  
  labs(
    title = str_glue("COVID-19 confirmed cases worldwide"),
    x = "Year 2020",
    y = "Cumulative Cases") +
  
  theme_minimal() +
  
  scale_x_date(date_breaks= "1 month", date_labels = "%B") +
  scale_colour_manual(values = c("#cb75e0", "#75e0d2", "#7ae075", "#dee075", "#e07575")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(minor_breaks = seq(0 , 15000000, 1250000),
                     breaks = seq(0,15000000, 2500000),
                     labels = unit_format(unit = "M", scale = 1e-6))+
  
  # Rearrange legend
  theme(legend.position="bottom") +
  guides(col = guide_legend(nrow = 2, byrow = TRUE))





