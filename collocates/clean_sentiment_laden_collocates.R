library(tidyverse)
library(lubridate)

property_collocates <- read_csv("~/projects/hansard-shiny/cleaned_collocates_sentiment_laden_noun_modifiers_property_keywords_07222021.csv")

decade <- 10

neutral <- 0

property_collocates <- property_collocates %>%
  mutate(decade = year - year %% decade) %>%
  select(-year)

property_collocates <- property_collocates %>%
  group_by(grammatical_collocates, decade) %>%
  add_count() %>%
  unique() %>%
  ungroup()

positive <- property_collocates %>%
  filter(combined_score > neutral)

positive$sentiment <- "Positive"

negative <- property_collocates %>%
  filter(combined_score < neutral)

negative$sentiment <- "Negative"

total <- bind_rows(positive, negative)

total <- total %>%
  select(n, sentiment, decade, grammatical_collocates)

write_csv(total, "~/projects/hansard-shiny/data/collocates/property_collocates.csv")
