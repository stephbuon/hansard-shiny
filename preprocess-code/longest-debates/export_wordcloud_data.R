library(tidyverse)
library(quanteda)
library(spacyr)

export_wordcloud_data <- function(tokenized_hansard, longest_debates) {
  
  tokenized_hansard <- tokenized_hansard %>%
    select(debate, debate_id, speechdate)

  stopwords <- stopwords %>% # be able to get rid of this 
    rename(ngrams = stop_word)
  
  longest_debates <- left_join(longest_debates, tokenized_hansard, by = c("debate", "debate_id", "speechdate"))
  
  rm(tokenized_hansard)
  
  longest_debates <- anti_join(longest_debates, stopwords, by = "ngrams")
  
  longest_debates <- longest_debates %>%
    rename(text = ngrams)
  
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
  
  all <- left_join(out, metadata, by = "doc_id")
  
  all <- all %>%
    group_by(speechdate, debate_id, debate, lemma) %>%
    count(speechdate, debate_id, debate, lemma) %>%
    rename(token_count = n) %>%
#     arrange(desc(token_count)) %>%
#     slice(1:20)
# 
# write_csv(all, "~/longest_debates_wordcloud.csv")
# 
# 
# library(tidyverse)
# 
# wc <- read_csv("~/projects/hansard-shiny/longest_debates_wordcloud.csv")
# 
# 
# a <- wc %>%
  group_by(speechdate, debate_id, debate) %>%
  arrange(desc(token_count)) %>%
  slice(1:60)
  
  fwrite(all, paste0(export_dir, "clean_longest_debates_wordcloud.csv")) }


hansard_tokens <- fread(paste0(preprocess_data_dir, "hansard_tokens_c19_improved_speaker_names_app_data.csv"))

longest_debates <- fread(paste0(preprocess_data_dir, "longest_debates.csv"))
export_wordcloud_data(hansard_tokens, longest_debates)

