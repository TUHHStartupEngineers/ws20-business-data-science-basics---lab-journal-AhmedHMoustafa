# Data Science at TUHH ------------------------------------------------------
# Challenge 3.2 ----

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

# Patent
col_types_p <- list(
  id = col_character(),
  type = col_skip(),
  number = col_skip(),
  country = col_skip(),
  date = col_date("%Y-%m-%d"),
  abstract = col_skip(),
  title = col_skip(),
  kind = col_skip(),
  num_claims = col_skip(),
  filename = col_skip(),
  withdrawn = col_skip()
)

patent_tbl <- vroom(
  file       = "patent.tsv", 
  delim      = "\t", 
  col_types  = col_types_p,
  na         = c("", "NA", "NULL")
)

# 3.0 Convert to data.table ----

# patent_assignee data

setDT(patent_assignee_tbl)

# assignee data

setDT(assignee_tbl)

# patent data

setDT(patent_tbl)

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
combined_data <- merge(x = combined_data_1, y = patent_tbl, 
                       by.x    = "patent_id", 
                       by.y    = "id",
                       all.x = FALSE, 
                       all.y = FALSE)
toc()


# Filter by type and date
type_2_data <- filter(combined_data, type == 2)

data_2019 <- filter(type_2_data, date > "2019-01-01" & date <"2019-12-31")

# Order by organization
setkey(data_2019, "organization")

setorderv(data_2019, "organization")

# Count
most_2019_patents <- data_2019 %>% count(organization, sort = TRUE, name = "patent_count")

top_10_2019_patents <- most_2019_patents[1:10]

top_10_2019_patents
