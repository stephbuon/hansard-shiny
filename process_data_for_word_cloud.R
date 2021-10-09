library(tidyverse)

stopwords <- read_csv("~/stopwords.csv") %>%
  rename(ngram = stop_word)
  
tokenized_hansard <- read_csv("/scratch/group/pract-txt-mine/tokenized_hansard.csv")

longest_debates <- read_csv("~/longest_debates.csv")

longest_debates <- left_join(longest_debates, tokenized_hansard, by = c("debate", "debate_id"))

rm(hansard)

longest_debates <- anti_join(longest_debates, stopwords, by = "ngram")

longest_debates <- longest_debates %>%
  group_by(speechdate, debate_id, debate, token) %>%
  count(speechdate, debate_id, debate, token) %>%
  rename(token_count = n) %>%
  slice_max(order_by = token_count, n = 20)

write_csv(longest_debates, "~/longest_debates_wordcloud.csv")