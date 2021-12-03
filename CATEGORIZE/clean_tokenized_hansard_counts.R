# five or more mentions

library(data.table)
library(tidyverse)

source("~/projects/hansard-shiny/preprocess/global_functions.R")

hansard <- fread("~/projects/hansard-shiny/tokenized_hansard_counts.csv")

stop_words <- import_stopwords_as_regex()

hansard <- hansard %>%
  filter(!str_detect(new_speaker, "[:digit:]\\.0")) %>%
  filter(!str_detect(new_speaker, "^[:digit:]"))

hansard <- hansard %>%
  filter(!str_detect(ngrams, "[[:digit:]]"))
  
hansard$ngrams <- str_replace(hansard$ngrams, "'s", "")
  
hansard <- hansard %>%
    filter(!str_detect(ngrams, stop_words))

h <- hansard %>%
  mutate(clean_new_speaker = new_speaker)

h$clean_new_speaker <- str_replace_all(h$clean_new_speaker, "_", " ") 
h$clean_new_speaker <- str_replace_all(h$clean_new_speaker, "[:digit:]", "")

h$clean_new_speaker <- str_to_title(h$clean_new_speaker)

write_csv(h, "~/projects/hansard-shiny/clean_clean_tokenized_hansard_counts.csv")
