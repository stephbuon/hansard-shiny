library(tidyverse)

add_decade_and_clean_speakers_col <- function(hansard, hansard_speakers) {
  
  hansard <- hansard %>%
    mutate(decade = year - year %% 10)
  
  hansard <- left_join(hansard, hansard_speakers, by = "sentence_id") 
  
  return(hansard) }
