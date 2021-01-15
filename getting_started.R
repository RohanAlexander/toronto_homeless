#### Preamble ####
# Purpose: Update the TidyTuesday data to include 2020
# Author: Rohan Alexander
# Email: rohan.alexander@utoronto.ca
# Date: 15 January 2021
# Prerequisites: -
# Issues: I have a hard time believing these numbers in 2020, but I don't know where I've made a mistake.
# To do: 
# - The data arrive regularly - no reason this can't automatically update into Shiny.
# - There is something funny going on in 2017, maybe with the dates


#### Workspace set-up ####
# Libraries
library(opendatatoronto)
library(tidyverse)
library(lubridate)

# Get the data
# Based on https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-12-01/readme.md
all_data <- 
  opendatatoronto::search_packages("Daily Shelter Occupancy") %>% 
  opendatatoronto::list_package_resources() %>% 
  dplyr::filter(name %in% c("daily-shelter-occupancy-2017-csv",
                     "daily-shelter-occupancy-2018-csv", 
                     "daily-shelter-occupancy-2019-csv", 
                     "daily-shelter-occupancy-2020.csv")) %>% 
  group_split(name) %>% # Don't totally get this
  map_dfr(get_resource, .id = "file")


#### Data cleaning and prep ####
# The main issue with the data is the dates. In 2017-2019 (inc) they are ymd, but
# for 2020 they are mdy. The separator is also inconsistent. The SUPER weird 
# thing is that they're not in order in the raw data, but I checked by 
# splitting them into pieces (day, month, year) and the counts seem okay; weird though.
toronto_shelters <-
  all_data %>% 
  janitor::clean_names() %>% # Make the column names easier to type
  mutate(occupancy_date = str_remove(occupancy_date, "T[:digit:]{2}:[:digit:]{2}:[:digit:]{2}"),
         occupancy_date = str_replace_all(occupancy_date, "/", "-")
  ) %>% # 1st line removes times (probs don't actually need to do) and 2nd makes the separation consistent
  mutate(date = case_when(
    file == "4" ~ mdy(occupancy_date, quiet = TRUE), 
    file %in% c("1", "2", "3") ~ ymd(occupancy_date, quiet = TRUE),
    TRUE ~ NA_Date_
    )
    ) %>% # The parsing is different depending on whether it's 2017-2019 or 2020. Last line is a catch-all - shouldn't get there.
  select(date, organization_name:capacity) %>% 
  rename(occupancy_date = date)

#### Analysis ####
# Interested in availability of shelter spots in Toronto on the basis of sector.
# Different sectors focus on different folks: Co-ed, Families, Men, Women, Youth.
# So for each day for each sector we have a proportion (note: horrifyingly >1 is possible).
# Based on: https://github.com/llendway/tidy_tuesday_in_thirty/blob/main/2020_12_01_tidy_tuesday.Rmd
usage_rate <- 
  toronto_shelters %>% 
  tidyr::drop_na(occupancy, capacity) %>% # We only want rows that have data for both occupancy and capacity
  group_by(occupancy_date, sector) %>% # We want to know the occupancy by date and sector
  summarise(the_sum = sum(occupancy),
            the_capacity = sum(capacity),
            the_usage = the_sum / the_capacity, .groups = 'drop')

# Graph 2017-2019 (inc)
usage_rate %>% 
  filter(year(occupancy_date) != "2020") %>% 
  ggplot(aes(x = occupancy_date, y = the_usage, color = sector)) + 
  # geom_smooth(aes(group = sector), se = FALSE) +
  geom_point(aes(group = sector), alpha = 0.3) +
  scale_y_continuous(limits = c(0, NA)) +
  labs(color = "Type",
       x = "Date",
       y = "Occupancy rate",
       title = "Toronto shelters",
       subtitle = "Occupancy per day") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1")


# Graph 2017-2020 (inc)
usage_rate %>% 
  ggplot(aes(x = occupancy_date, y = the_usage, color = sector)) + 
  # geom_smooth(aes(group = sector), se = FALSE) +
  geom_point(aes(group = sector), alpha = 0.3) +
  scale_y_continuous(limits = c(0, NA)) +
  labs(color = "Type",
       x = "Date",
       y = "Occupancy rate",
       title = "Toronto shelters",
       subtitle = "Occupancy per day") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1")

# Quoted from Kieran Healy, 2020, The Kitchen Counter Observatory, 21 May, 
# https://kieranhealy.org/blog/archives/2020/05/21/the-kitchen-counter-observatory/
## "...With a laptop, some free software, and a cup of coffee, I can examine what ought 
## to seem like a staggering amount of information. ...I sit here at home, surveying 
## the scope of what's being inflicted on people across the country and around the 
## world as this disease spreads. ...
# ... People sometimes think (or complain) that working with quantitative data like this 
# inures you to the reality of the human lives that lie behind the numbers. Numbers 
# and measures are crude; they pick up the wrong things; they strip out the meaning 
# of what's happening to real people; they make it easy to ignore what can't be counted. 
# There's something to those complaints. But it's mostly a lazy critique. In practice, 
# I find that far from distancing you from questions of meaning, quantitative data 
# forces you to confront them. The numbers draw you in. Working with data like this 
# is an unending exercise in humility, a constant compulsion to think through what 
# you can and cannot see, and a standing invitation to understand what the measures 
# really capture—what they mean, and for whom. ..."




#### Investigate 2017 ####
# Raw numbers
usage_rate %>% 
  # filter(year(occupancy_date) != "2020") %>% 
  ggplot(aes(x = occupancy_date, y = the_sum, color = sector)) + 
  # geom_smooth(aes(group = sector), se = FALSE) +
  geom_point(aes(group = sector), alpha = 0.3) +
  scale_y_continuous(limits = c(0, NA)) +
  labs(color = "Type",
       x = "Date",
       y = "Occupancy rate",
       title = "Toronto shelters",
       subtitle = "Occupancy per day") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1")

# Investigate
# Read in the data as was for TidyTuesday
shelters <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-12-01/shelters.csv')

shelters %>% 
  filter(sector == "Families") %>% 
  # group_by(occupancy_date) %>% 
  # summarize(total_capacity = sum(capacity, na.rm = TRUE),
  #           total_occupancy = sum(occupancy, na.rm = TRUE),
  #           occupancy_rate = total_occupancy/total_capacity) %>% 
  # filter(occupancy_rate != 0) %>% 
  ggplot(aes(x = occupancy_date, y = occupancy)) + 
  # geom_smooth(aes(group = sector), se = FALSE) +
  geom_point(alpha = 0.3)