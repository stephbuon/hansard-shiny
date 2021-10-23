# five or more mentions

library(data.table)
library(tidyverse)

hansard <- fread("~/projects/hansard-shiny/tokenized_hansard_counts.csv")

stop_words <- read_csv("~/projects/hansard-shiny/stopwords_text2vec.csv")

stop_words <- stop_words %>%
  summarise(stop_word = paste0(stop_word, collapse="|"))

stop_words <- stop_words$stop_word

hansard <- hansard %>%
  filter(!str_detect(new_speaker, "[:digit:]\\.0")) %>%
  filter(!str_detect(new_speaker, "^[:digit:]"))

hansard <- hansard %>%
  filter(!str_detect(ngrams, "[[:digit:]]"))
  
hansard$ngrams <- str_replace(hansard$ngrams, "'s", "")
  
hansard <- hansard %>%
    filter(!str_detect(ngrams, stop_words))

write_csv(hansard, "~/projects/hansard-shiny/clean_tokenized_hansard_counts.csv")



h <- read_csv("~/projects/hansard-shiny/clean_tokenized_hansard_counts.csv")

h <- h %>%
  mutate(clean_new_speaker = new_speaker)

h$clean_new_speaker <- str_replace_all(h$clean_new_speaker, "_", " ") 
h$clean_new_speaker <- str_replace_all(h$clean_new_speaker, "[:digit:]", "")

h$clean_new_speaker <- str_to_title(h$clean_new_speaker)

write_csv(h, "~/projects/hansard-shiny/clean_clean_tokenized_hansard_counts.csv")
