library(tidyverse)

top_speakers <- read_csv("/scratch/group/pract-txt-mine/sbuongiorno/top_speakers_by_decade_improved_speaker_names.csv") %>%
  select(-sp_ranking_decade, -top_speaker)

tokenized_hansard <- read_csv("/scratch/group/pract-txt-mine/sbuongiorno/tokenized_hansard_improved_speaker_names_w_decade.csv")

out <- left_join(top_speakers, tokenized_hansard, by = c("new_speaker", "decade"))

speaker_count <- out %>%
  count(decade, new_speaker, ngrams)

write_csv(speaker_count, "~/speaker_comparison_speaker_count.csv")




stopwords <- read_csv("/scratch/group/pract-txt-mine/sbuongiorno/stopwords_text2vec.csv") %>%
  rename(ngrams = stop_word)

stopwords <- stopwords %>%
  summarise(all = paste0(ngrams, collapse="|"))

stopwords <- stopwords$all



speaker_count_2 <- speaker_count %>%
  filter(!grepl("[[:digit:]]", ngrams))

speaker_count_2$ngrams <- str_replace(speaker_count_2$ngrams, "'s", "")

speaker_count_2 <- speaker_count_2 %>%
  filter(!str_detect(ngrams, stopwords))



speaker_count_2 <- speaker_count_2 %>%
  filter(n > 4)

test <- speaker_count_2 %>%
  filter(decade == 1870)

write_csv(speaker_count_2, "~/speaker_comparison_speaker_count_for_app.csv")
