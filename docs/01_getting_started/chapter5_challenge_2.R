# Data Science at TUHH ------------------------------------------------------
# Challenge 4.1 ----

# 1.0 Load libraries ----
library(tidyverse)
library(maps)

# 2.0 Import ----

covid_data_tbl <- read_csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv")

world <- map_data("world") 

# 3.0 Data wrangling ----

covid_tbl <- covid_data_tbl %>%
  
  # Select relevant columns
  select(deaths, popData2019, countriesAndTerritories) %>%
  
  # Calculate total deaths
  group_by(countriesAndTerritories) %>%
  mutate(total_deaths = sum(deaths)) %>%
  ungroup() %>%
  
  # Clean up table
  select(popData2019, countriesAndTerritories, total_deaths) %>%
  unique() %>%
  
  # Add deaths with respect to total population
  mutate(deaths_percent = total_deaths/popData2019) %>%
  
  #clean up
  select(countriesAndTerritories, deaths_percent) %>% 
  
  # Handle differences in country names
  mutate(across(countriesAndTerritories, str_replace_all, "_", " ")) %>%
  mutate(countriesAndTerritories = case_when(
    
    countriesAndTerritories == "United Kingdom" ~ "UK",
    countriesAndTerritories == "United States of America" ~ "USA",
    countriesAndTerritories == "Czechia" ~ "Czech Republic",
    TRUE ~ countriesAndTerritories
    
  ))

# Merge data
combined_data <- merge(x = covid_tbl, y = world, 
                         by.x    = "countriesAndTerritories", 
                         by.y    = "region",
                         all.x = FALSE, 
                         all.y = FALSE)

# 4.0 Visualization ----

ggplot(combined_data) +
  
  #Data representation & Legend 
  scale_fill_gradient(low = "red", high = "black", name = "Mortality Rate",
                      n.breaks = 4) +
  
  # Apply base layer for countries without data
  geom_map(dat=world, map = world,
           aes(map_id=region), fill="grey", color="white") +
  
  
  # Apply main map data layer
  geom_map(aes(fill = deaths_percent, map_id = countriesAndTerritories), map = world,
           color = "#ffffff", size=0.000001) +
  
  expand_limits(x = world$long, y = world$lat) +
  
  labs(
    title = "Confirmed COVID-19 deaths relative to the size of the population",
    subtitle = "More than 1.2 Million confirmed deaths worldwide",
    caption = "Date: 06-12-2020",
    x = "",
    y = ""
  ) + 
  
  # Remove Axis labels (long & lat)
  theme(axis.text.x=element_blank()) +
  theme(axis.text.y=element_blank())

