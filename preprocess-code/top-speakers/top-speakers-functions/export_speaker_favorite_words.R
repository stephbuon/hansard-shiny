library(tidyverse)
library(tidytext)

export_top_speakers_favorite_words <- function(top_speakers, stopwords) {
  
  stopwords <- stopwords %>%
    rename(word = stop_word)
  
  speaker_names_list <- top_speakers$disambig_speaker

  decades <- c(1800, 1810, 1820, 1830, 1840, 1850, 1860, 1870, 1880, 1890, 1900, 1910)
  
  output <- data.frame()
  
  for(decade in 1:length(decades)) {
    
    hansard <- fread(preprocess_data_dir, paste0("hansard_tokens_decades_", decade, ".csv")) %>%
      select(disambig_speaker, text, year)
    
    for(speaker_name in 1:length(speaker_names_list)) {

      filtered_speaker_name <- hansard %>%
        filter(str_detect(speaker, regex(speaker_name, ignore_case = TRUE))) %>%
        filter(decade == decade)
      
      filtered_speaker_name$disambig_speaker <- speaker_name # to make names consistent
      
      filtered_speaker_name <- filtered_speaker_name %>%
        anti_join(stopwords)
      
      filtered_speaker_name <- filtered_speaker_name %>%
        count(disambig_speaker, word, decade) %>%
        arrange(desc(n)) %>%
        slice(1:20)
      
      output <- bind_rows(output, filtered_speaker_name) } }
  
  output <- unique(output) # bc speaker can be mentioned more than once
  
  return(output) }