library(tidyverse)

hansard_triples <- read_csv("/scratch/group/pract-txt-mine/c19_hansard_debate_text_triples_07232021.csv") %>% # remember to replace the speaker
  rename(sentence_id = doc_id) %>%
  select(sentence_id, speechdate, triple, debate, year)

hansard_speakers <- read_csv("/scratch/group/pract-txt-mine/hansard_c19_12192019.csv") %>%
  select(sentence_id, new_speaker)

hansard_triples <- left_join(hansard_triples, hansard_speakers, by = "sentence_id")

rm(hansard_speakers)

hansard_triples <- hansard_triples %>%
  drop_na(triple)

write_csv(hansard_triples, "~/speaker_triples.csv")
