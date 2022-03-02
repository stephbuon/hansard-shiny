library(dplyr)
library(stringr)
library(data.table)

import_stopwords_as_regex <- function() {
  
  stopwords <- read_csv("/scratch/group/pract-txt-mine/sbuongiorno/stopwords_text2vec.csv") 
  
  stopwords <- stopwords %>%
    summarise(all = paste0(stop_word, collapse="|"))
  
  stopwords <- stopwords$all 
  
  return(stopwords) }


clean_data_for_word_embeddings <- function(data_decade_subset) {
  
  data_decade_subset <- data_decade_subset %>%
    filter(!str_detect(ngrams, "[[:digit:]]"))
  
  data_decade_subset$ngrams <- str_replace(data_decade_subset$ngrams, "'s", "")
  
  stopwords <- import_stopwords_as_regex()
  
  data_decade_subset <- data_decade_subset %>%
    filter(!str_detect(ngrams, stopwords))
  
  return(data_decade_subset) }



#input_dir <- "/scratch/group/pract-txt-mine/sbuongiorno/tokenized_hansard_w_metadata"
#export_dir <- "/scratch/group/pract-txt-mine/sbuongiorno/tokenized_hansard_count"

#dir.create(file.path(export_dir))


export_speech_length_type_data <- function(preprocess_data_dir) {
  
  files <- list.files(path = preprocess_data_dir, pattern = "hansard_tokens_decades_(.*).csv", full.names = TRUE)
  
  decades <- c("1800", "1810", "1820", "1830", "1840", "1850", "1860", "1870", "1880", "1890", "1900", "1910")
  
  for(file in files) {
    
    for(d in decades) {
      
      if(str_detect(file, d)) {
        
        tokenized_decade <- fread(file)
        
        tokenized_decade <- tokenized_decade %>%
          select(speech_id, ngrams, decade)
        
        tokenized_decade <- tokenized_decade %>%
          add_count(speech_id) %>%
          rename(speech_length = n)
        
        short_speech <- tokenized_decade %>%
          filter(speech_length > 0,
                 speech_length < 50) %>%
          mutate(speech_length_type = 0)
        
        mid_range_speech <- tokenized_decade %>%
          filter(speech_length > 49,
                 speech_length < 1000) %>%
          mutate(speech_length_type = 1)
        
        long_speech <- tokenized_decade %>%
          filter(speech_length > 999) %>%
          mutate(speech_length_type = 2)
        
        
        tokenized_decade <- bind_rows(short_speech, mid_range_speech, long_speech)
        
        tokenized_decade$ngrams <- str_replace(tokenized_decade$ngrams, "'s", "")
        
        tokenized_decade <- tokenized_decade %>%
          group_by(speech_length_type, decade) %>% # added
          count(ngrams) %>%
          rename(token_count = n)
        
        tokenized_decade <- tokenized_decade %>%
          filter(token_count > 10) 
        
        tokenized_decade <- clean_data_for_word_embeddings(tokenized_decade)
        
        speech_length_types <- c(0, 1, 2)
        
        
        for (i in speech_length_types) {
          
          output <- tokenized_decade %>%
            filter(speech_length_type == i)
          
          output <- output %>%
            select(-speech_length_type)
          
          output <- unique(output)
          
          fwrite(output, file.path(paste0(export_dir, "/tokenized_hansard_count_", d, "_speech_length_type_", i , ".csv")))
          
        }
        
        
        
        
      }
      
    }
    
  }
  
  
  
  
  #export_dir <- "/home/stephbuon/projects/hansard-shiny/app-data/debate-text/tokenized_hansard_count"

  files <- list.files(path = preprocess_data_dir, pattern = "tokenized_hansard_count_(.*)_speech_length_type_(.*).csv", full.names = TRUE)
  
  speech_length_types <- c("0", "1", "2")
  
  for(i in speech_length_types) {
    out <- data.frame()
    
    for(file in files) {
      
      if(str_detect(file, paste0("type_", i))) { 
        
        tokenized_decade <- fread(file)
        
        out <- bind_rows(out, tokenized_decade)
        
        
      }
      
      
      
      
    }
    out <- unique(out)
    fwrite(out, paste0(export_dir, "/tokenized_hansard_count_all_decades_speech_length_type_", i , ".csv"))
    
    
    
  }
  
  
  
  
  
}

