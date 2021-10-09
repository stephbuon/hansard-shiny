library(tidyverse)
library(lubridate)

b = 5
d = 5

tokenized_hansard <- read_csv("/scratch/group/pract-txt-mine/tokenized_hansard.csv")

metadata <- tokenized_hansard %>%
  select(speaker, speechdate, speech_id, debate, sentence_id) %>%
  distinct()

# what were the longest debates?
debate_length <- tokenized_hansard %>%
  group_by(speechdate, debate_id) %>% # changed this from debate
  summarize(words_per_debate = n()) %>%
  ungroup() %>%
  mutate(year = year(speechdate)) %>%
  mutate(decade = 10*floor(year/10))

debate_length <- debate_length %>% 
  group_by(decade) %>%
  mutate(decade_ranking = rank(desc(words_per_debate))) %>%
  ungroup() %>%
  group_by(year) %>%
  mutate(year_ranking = rank(desc(words_per_debate))) %>%
  ungroup() %>%
  inner_join(metadata) 

longest_debates_in_decade <- debate_length %>%
  #  group_by(5*round(year(speechdate)/5)) %>%
  filter(decade_ranking <= 5) %>%
  inner_join(metadata) %>%
  select(-speaker, -speech_id) %>%
  distinct() 

write_csv(longest_debates_in_decade, "longest_debates.csv")

