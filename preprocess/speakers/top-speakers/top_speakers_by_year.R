library(tidyverse)
library(lubridate)

b = 5

tidy_hans <- read_csv("tokenized_hansard.csv")

words_per_day <- tidy_hans %>%
  group_by(speechdate, speaker) %>%
  summarize(words_per_day = n())

words_per_day <- words_per_day %>%
  mutate(sp_ranking = rank(desc(words_per_day))) %>%
  mutate(year = year(speechdate)) %>%
  mutate(decade =10* floor(year/10)) %>%
  group_by(year) %>%
  mutate(sp_ranking_year = rank(desc(words_per_day))) %>%
  mutate(max_words = max(words_per_day)) %>%
  ungroup() %>%
  #group_by(decade) %>%
  #mutate(sp_ranking_decade = rank(desc(words_per_day))) %>%
  #ungroup() %>%
  mutate(top_speaker = if_else(sp_ranking_year <= b, TRUE, FALSE))

top_speakers <- words_per_day %>%
  filter(top_speaker == TRUE)

write_csv(top_speakers, "top_speakers_by_year.csv")
