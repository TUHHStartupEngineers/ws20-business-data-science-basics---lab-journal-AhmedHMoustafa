# Data Science at TUHH ------------------------------------------------------
# Challenge 3.1 ----

# 1.0 Load libraries ----

# Tidyverse
library(tidyverse)
library(vroom)

# Data Table
library(data.table)

# Counter
library(tictoc)

# 2.0 Import ----

# Patent_Assignee
col_types_pa <- list(
  patent_id = col_character(),
  assignee_id = col_character(),
  location_id = col_skip()
)

patent_assignee_tbl <- vroom(
  file       = "patent_assignee.tsv", 
  delim      = "\t", 
  col_types  = col_types_pa,
  na         = c("", "NA", "NULL")
)

# Assignee
col_types_a <- list(
  id = col_character(),
  type = col_integer(),
  name_first = col_skip(),
  name_last = col_skip(),
  organization = col_character()
)

assignee_tbl <- vroom(
  file       = "assignee.tsv", 
  delim      = "\t", 
  col_types  = col_types_a,
  na         = c("", "NA", "NULL")
)

# 3.0 Convert to data.table ----

# patent_assignee Data

setDT(patent_assignee_tbl)

# assignee Data

setDT(assignee_tbl)

# 4.0 Data wrangling ----

# Joining / Merging Data 

tic()
combined_data <- merge(x = patent_assignee_tbl, y = assignee_tbl, 
                       by.x    = "assignee_id", 
                       by.y    = "id",
                       all.x = FALSE, 
                       all.y = FALSE)
toc()

# Filter by type
type_2_data <- filter(combined_data, type == 2)

# Order by organization
setkey(type_2_data, "organization")

setorderv(type_2_data, "organization")

# Count

most_us_patents <- type_2_data %>% count(organization, sort = TRUE, name = "patent_count")

top_10_us_patents = most_us_patents[1:10]
top_10_us_patents
