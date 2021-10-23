library(tidyverse)
library(lubridate)

dir <- "/scratch/group/pract-txt-mine/sbuongiorno/"

tokenized_hansard <- read_csv(paste0(dir, "tokenized_hansard.csv")) %>%
  select(speechdate, speech_id, ngrams)

tokenized_hansard <- tokenized_hansard %>%
  mutate(year = year(speechdate)) %>%
  mutate(decade =10* floor(year/10)) %>%
  select(-year, -speechdate)

tokenized_hansard_count <- tokenized_hansard %>%
  count(speech_id, decade)

write_csv(tokenized_hansard_count, paste0(dir, "speech_lengths.csv"))

