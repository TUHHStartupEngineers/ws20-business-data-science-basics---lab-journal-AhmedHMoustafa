# Data Science at TUHH ------------------------------------------------------
# Challenge 3.3 ----

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

# uspc 
col_types_u <- list(
  uuid = col_skip(),
  patent_id = col_character(),
  mainclass_id = col_character(),
  subclass_id = col_skip(),
  sequence = col_skip()
)

uspc_tbl <- vroom(
  file       = "uspc.tsv", 
  delim      = "\t", 
  col_types  = col_types_u,
  na         = c("", "NA", "NULL")
)

# 3.0 Convert to data.table ----

# patent_assignee data

setDT(patent_assignee_tbl)

# assignee data

setDT(assignee_tbl)

# uspc data

setDT(uspc_tbl)

# 4.0 Data wrangling ----

# Joining / Merging Data 

tic()
combined_data_1 <- merge(x = patent_assignee_tbl, y = assignee_tbl, 
                         by.x    = "assignee_id", 
                         by.y    = "id",
                         all.x = FALSE, 
                         all.y = FALSE)
toc()

tic()
combined_data <- merge(x = combined_data_1, y = uspc_tbl, 
                       by  = "patent_id",
                       all.x = FALSE, 
                       all.y = FALSE)
toc()

# Filter by type
worldwide_data <- filter(combined_data, type == 2 | type == 3)

# Order by organization
setkey(worldwide_data, "organization")

setorderv(worldwide_data, "organization")

# Cleaning
result_0 <- unique(worldwide_data, by = "patent_id")

# Top 10 worldwide
result_1 <- result_0 %>% count(organization, sort = TRUE, name = "patent_count")
result_2 <- result_1[1:10]

# Their patents
result_3 <- semi_join(result_0, result_2, by = "organization")
result_4 <- result_3 %>% count(mainclass_id, sort = TRUE, name = "count")

# Top 5 USPTO patent main classes
result_5 <- result_4[1:5]
