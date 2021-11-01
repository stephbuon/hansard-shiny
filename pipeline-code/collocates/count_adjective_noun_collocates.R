library(tidyverse)

adjective_noun_collocates <- read_csv("~/all_collocates.csv")

decade <- 10

adjective_noun_collocates <- adjective_noun_collocates %>%
  mutate(decade = year - year %% decade)

adjective_noun_collocates <- adjective_noun_collocates %>%
  count(grammatical_collocates, afinn, textblob, vader, decade)

write_csv(adjective_noun_collocates, "~/clean_all_adjective_noun_collocates.csv")
