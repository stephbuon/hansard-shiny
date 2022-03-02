library(tidyverse)
library(lubridate)

find_speech_length <- function(preprocess_data_dir, hansard) {
  
  tokenized_hansard <- read_csv(paste0(dir, "tokenized_hansard.csv")) %>%
    select(speechdate, speech_id, ngrams)
  
  tokenized_hansard <- tokenized_hansard %>%
    mutate(year = year(speechdate)) %>%
    mutate(decade =10* floor(year/10)) %>%
    select(-year, -speechdate)
  
  tokenized_hansard_count <- tokenized_hansard %>%
    count(speech_id, decade)
  
  write_csv(tokenized_hansard_count, paste0(preprocess_data_dir, "speech_lengths.csv")) 
  
  
  }

