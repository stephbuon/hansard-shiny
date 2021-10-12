library(tidyverse)

hansard <- read_csv("/scratch/group/pract-txt-mine/tokenized_hansard.csv") %>%
  select(ngrams, speechdate)


hansard <- hansard %>%
  mutate(year = year(as.Date(hansard$speechdate))) %>%
  select(-speechdate)

hansard$ngram_order <- seq.int(nrow(hansard))

write_csv(hansard, "/scratch/group/pract-txt-mine/sbuongiorno/clean_tokenized_hansard.csv")
