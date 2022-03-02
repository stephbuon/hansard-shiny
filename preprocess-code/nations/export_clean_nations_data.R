library(tidyverse)

export_clean_nations_data <- function(preprocess_data_dir, nations) {
  
  nations <- read_csv(nations)
  
  nations <- unique(nations)
  
  nations <- nations %>%
    filter(!str_detect(nations, "\\'s'")) # get rid of possessive nations
  
  write_csv(nations, paste0(preprocess_data_dir, "nations.csv")) 
  
  return(nations) }

hansard <- fread(paste0(preprocess_data_dir, "hansard_tokens_c19_improved_speaker_names_app_data.csv"))
original_nations_data <- fread(paste0(preprocess_data_dir, "origin-data/keywords_lists/collaborative_nations.csv"))

export_clean_nations_data(preprocess_data_dir, original_nations_data)



