# Data Science at TUHH ------------------------------------------------------
# Challenge 2.2 ----

# 1.0 Load libraries ----
library(tidyverse) # Main Package - Loads dplyr, purrr, etc.
library(rvest)     # HTML Hacking & Web Scraping
library(xopen)     # Quickly opening URLs
library(jsonlite)  # converts JSON files to R objects
library(glue)      # concatenate strings
library(stringi)   # character string/text processing

# 2.0 Extract URLs ----
url_category <- "https://www.rosebikes.de/fahrr%C3%A4der/trekking"
html_category        <- read_html(url_category)

bike_url_tbl <- html_category %>%
  
  # Going further down the tree and select nodes by class
  # Selecting two classes makes it specific enough
  html_nodes(css = ".align-middle > a") %>%
  html_attr("href") %>%
  
  # Convert vector to tibble
  enframe(name = "position", value = "subdirectory") %>%
  
  # Add the domain, because we will get only the subdirectories
  mutate(
    url = glue("https://www.rosebikes.de{subdirectory}")
  ) %>%
  
  # Some categories are listed multiple times.
  # We only need unique values
  distinct(url)

bike_url_tbl

# 3.0 Data acquisition function ----

get_bike_data <- function(url) {
  
html_category  <- read_html(url)
bike_url_tbl        <- html_category %>%
  
  # Get the 'a' nodes, which are hierarchically underneath 
  html_nodes(css = ".catalog-category-models__model > a") %>%
  html_attr('href') %>%
  
  # Convert vector to tibble
  enframe(name = "position", value = "url")

# Add model name
bike_table <- bike_url_tbl %>% 
  mutate(model = html_category %>%
           html_nodes(css = ".catalog-category-model__title")%>% html_text())

# Add price
bike_info_tbl <- bike_table %>% 
  mutate(price = html_category %>%
           html_nodes(css = ".catalog-category-model__price-current")%>% html_text())
}

# 4.0 Wrapping & running ----

# Extract the urls as a character vector
bike_category_url_vec <- bike_url_tbl %>% 
  pull(url)

# Run the function with every url as an argument
bike_data_lst <- map(bike_category_url_vec, get_bike_data)

# Merge the list into a tibble
bike_data_tbl <- bind_rows(bike_data_lst)

bike_data_tbl

#saveRDS(bike_data_tbl, "bike_data_tbl.rds")