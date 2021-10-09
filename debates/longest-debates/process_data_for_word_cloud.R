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
  arrange(desc(token_count)) %>%
  slice(1:20)

write_csv(longest_debates, "~/longest_debates_wordcloud.csv")




### maybe replace with this: 
library(tidyverse)
library(quanteda)
library(spacyr)

stopwords <- read_csv("~/stopwords.csv") %>%
  rename(ngrams = stop_word)

tokenized_hansard <- read_csv("~/hansard_samples/tokenized_hansard_sample.csv") %>% #("/scratch/group/pract-txt-mine/tokenized_hansard.csv") %>%
  select(debate, debate_id, speechdate, ngrams)

longest_debates <- read_csv("~/longest_debates.csv")

longest_debates <- left_join(longest_debates, tokenized_hansard, by = c("debate", "debate_id", "speechdate"))

rm(tokenized_hansard)

longest_debates <- anti_join(longest_debates, stopwords, by = "ngrams")

longest_debates <- longest_debates %>%
  rename(text = ngrams)

# do lemmatization here 

#longest_debates <- longest_debates %>%
#  drop_na()

longest_debates$doc_id <- seq.int(nrow(longest_debates))
longest_debates$doc_id <- paste0("doc_id_", longest_debates$doc_id)

metadata <- longest_debates %>%
  select(doc_id, speechdate, debate_id, debate, words_per_debate, year, decade, decade_ranking, year_ranking)

longest_debates <- corpus(longest_debates, docid_field = "doc_id")

out <- spacy_parse(longest_debates,
                   dependency = F,
                   lemma = T, 
                   pos = F,
                   entity = F) %>%
  select(-sentence_id)

all <- left_join(out, metadata)

all <- all %>%
  group_by(speechdate, debate_id, debate, lemma) %>%
  count(speechdate, debate_id, debate, lemma) %>%
  rename(token_count = n) %>%
  arrange(desc(token_count)) %>%
  slice(1:20)

write_csv(longest_debates, "~/longest_debates_wordcloud.csv")

