library(tidyverse)
library(lubridate)

b = 5

tidy_hans <- read_csv("/scratch/group/pract-txt-mine/sbuongiorno/tokenized_hansard_improved_speaker_names.csv")

tidy_hans <- tidy_hans %>%
  filter(!str_detect(new_speaker, "^NA$"))

tidy_hans <- tidy_hans %>%
  mutate(year = year(as.Date(tidy_hans$speechdate)))

decade <- 10

tidy_hans <- tidy_hans %>%
  mutate(decade = year - year %% decade)

tidy_hans <- tidy_hans %>%
  select(-year, -speechdate)

words_per_day <- tidy_hans %>%
  group_by(decade, new_speaker) %>%
  summarize(words_per_day = n())

words_per_day <- words_per_day %>%
  group_by(decade) %>%
  mutate(sp_ranking_decade = rank(desc(words_per_day))) %>%
  ungroup() %>%
  mutate(top_speaker = if_else(sp_ranking_decade <= b, TRUE, FALSE))

top_speakers <- words_per_day %>%
  filter(top_speaker == TRUE)

write_csv(top_speakers, "/scratch/group/pract-txt-mine/sbuongiorno/top_speakers_by_decade_improved_speaker_names.csv")

