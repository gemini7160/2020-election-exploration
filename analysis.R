# Analyzing Election Results

# Set Up
library(tidyverse)
raw_data <- read.csv("https://raw.githubusercontent.com/alex/nyt-2020-election-scraper/master/all-state-changes.csv")

# Basic data frame exploration
num_cols <- ncol(data)
num_rows <- nrow(data)
num_states <- length(unique(data$state))
num_timestamps <- length(unique(data$timestamp))

# Formatting: splitting out state name from electoral votes
data <- raw_data %>%
  separate(state, into=c("state", "ev"), " \\(") %>%
  mutate(ev = parse_number(ev))

# The number of time stamps varies for each state.
# How many reported time stamps exist for each state?
timestamps_by_state <- data %>%
  group_by(state) %>%
  count()

# When did Biden take the lead in Georgia?
ga_lead_time <- data %>%
  filter(state == "Georgia", leading_candidate_name == "Biden") %>%
  filter(timestamp == min(timestamp)) %>%
  pull(timestamp)

# What is the difference in votes in each state?


# How do total votes change over time (by candidate)?

