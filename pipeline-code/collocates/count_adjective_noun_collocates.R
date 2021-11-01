library(tidyverse)

adjective_noun_collocates <- read_csv("~/all_collocates.csv")

decade <- 10

adjective_noun_collocates <- adjective_noun_collocates %>%
  mutate(decade = year - year %% decade)

adjective_noun_collocates <- adjective_noun_collocates %>%
  count(grammatical_collocates, afinn, textblob, vader, decade)

adjective_noun_collocates <- adjective_noun_collocates %>%
  filter(!str_detect(grammatical_collocates, "^[:punct:] "))

adjective_noun_collocates$grammatical_collocates <- str_replace(adjective_noun_collocates$grammatical_collocates, "[:punct:]", "")

adjective_noun_collocates$grammatical_collocates <- str_to_title(adjective_noun_collocates$grammatical_collocates)

write_csv(adjective_noun_collocates, "~/clean_all_adjective_noun_collocates.csv")