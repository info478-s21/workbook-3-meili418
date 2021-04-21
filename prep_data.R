# Prep data

# Set up - make sure to set your working directory using RStudio
library(tidyr)
library(dplyr)
library(readxl)

# Load HALE data
hale <- read.csv("./data/raw/hale.csv", stringsAsFactors = FALSE) %>%
  rename(hale = val) %>%
  select(location_id, year, hale)

# Load life expectancy data
life_expectancy <- read.csv(
  "./data/raw/life_expectancy.csv",
  stringsAsFactors = FALSE
) %>%
  rename(le = val) %>%
  select(location_id, year, le)

# Load DALY data
dalys <- read.csv("./data/raw/dalys.csv", stringsAsFactors = FALSE) %>%
  rename(dalys = val) %>%
  select(location_id, year, dalys)

# Join data
all_data <- life_expectancy %>%
  left_join(hale, by = c("year", "location_id")) %>%
  left_join(dalys, by = c("year", "location_id"))


# Load hierarchy of locations (to exclude estimates at aggregate levels)
location_hierarchy <- read_excel("./data/raw/location_hierarchy.XLSX") %>%
  filter(level == 3) %>% # only country level
  select(location_id, location_name)

# Merge data, exclude aggregate locations
prepped <- left_join(location_hierarchy, all_data, by = "location_id") %>%
  select(location_name, year, hale, le, dalys)

# Create `data/prepped/` folder
dir.create("data/prepped", showWarnings = FALSE)

# Write data
write.csv(prepped, "./data/prepped/all_data.csv", row.names = FALSE)