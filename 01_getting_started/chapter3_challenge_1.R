# Data Science at TUHH ------------------------------------------------------
# Challenge 2.1 ----

# 1.0 Load libraries ----
library(tidyverse)
library(httr)
library(jsonlite)

# 2.0 Request Data ----
resp <- GET('https://picsum.photos/v2/list?page=2&limit=50')

#3.0 Convert JSON to Data Structure ----
data <- resp  %>% 
  .$content %>% 
  rawToChar() %>% 
  fromJSON()

data

