library(tidyverse)

stopwords <- read_csv("~/stopwords.csv") %>%
  rename(ngrams = stop_word)

tokenized_hansard <- read_csv("/scratch/group/pract-txt-mine/tokenized_hansard.csv") %>%
  select(debate, debate_id, speechdate, ngrams)

longest_debates <- read_csv("~/longest_debates.csv")

longest_debates <- left_join(longest_debates, tokenized_hansard, by = c("debate", "debate_id", "speechdate"))

rm(tokenized_hansard)

longest_debates <- anti_join(longest_debates, stopwords, by = "ngrams")

longest_debates <- longest_debates %>%
  group_by(speechdate, debate_id, debate, ngrams) %>%
  count(speechdate, debate_id, debate, ngrams) %>%
  rename(token_count = n) %>%
  slice(order_by = token_count, n = 20)

write_csv(longest_debates, "~/longest_debates_wordcloud.csv")
