# Analyzing Election Results

# Set Up
library(tidyverse)
raw_data <- read.csv("https://raw.githubusercontent.com/alex/nyt-2020-election-scraper/master/all-state-changes.csv")

# Basic data frame exploration
num_cols <- ncol(data)
num_rows <- nrow(data)
num_states <- length(unique(raw_data$state))
num_timestamps <- length(unique(raw_data$timestamp))

# Formatting:
# Splitting out state name from electoral votes
# Add Biden and Trump vote columns
data <- raw_data %>%
  separate(state, into=c("state", "ev"), " \\(") %>%
  mutate(ev = parse_number(ev)) %>%
  mutate(biden_votes =
           if_else(leading_candidate_name == "Biden", # condition
                   leading_candidate_votes,           # if true
                   trailing_candidate_votes,          #if false
                   ),
         trump_votes = total_votes_count - biden_votes
         )

# Quick check
check <- data %>%
  mutate(total_check = trump_votes + biden_votes,
         done_correctly = if_else(total_check == total_votes_count, 1, 0)) %>%
  summarize(total_correct = sum(done_correctly))

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

# What is the earliest time in each state that Biden is ahead?
biden_lead_time <- data %>%
  group_by(state) %>%
  filter(leading_candidate_name == "Biden") %>%
  filter(timestamp == min(timestamp)) %>%
  select(state, timestamp)

# What is the difference in votes in each state?
# (at the most recent time stamp)
vote_diff <- data %>%
  group_by(state) %>%
  filter(timestamp == max(timestamp)) %>%
  mutate(vote_diff = biden_votes - trump_votes,
         pct_diff = vote_diff / total_votes_count)

vote_diff_plot <- ggplot(vote_diff) +
  geom_col(mapping = aes(x = vote_diff,
                         y = reorder(state, vote_diff),
                         fill = leading_candidate_name)) +
  scale_fill_manual(values = c("blue", "red")) +
  labs(x = "Vote Difference",
       y = "State",
       fill = "Candidate",
       title = "Vote Difference",
       subtitle = "(at Most Recent Time Stamp)") +
  scale_x_continuous(labels = scales::comma)

vote_pct_plot <- ggplot(vote_diff) +
  geom_col(mapping = aes(x = pct_diff,
                         y = reorder(state, pct_diff),
                         fill = leading_candidate_name)) +
  scale_fill_manual(values = c("blue", "red")) +
  labs(x = "Vote Percent Difference",
       y = "State",
       fill = "Candidate",
       title = "Vote Percent Difference",
       subtitle = "(at Most Recent Time Stamp)")

# How do total votes change over time (by candidate)?

