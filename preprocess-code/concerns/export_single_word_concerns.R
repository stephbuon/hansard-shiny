library(tidyverse)

export_clean_concerns_data <- function(concerns) {
  
  concerns <- read_csv(concerns)
  
  concerns <- unique(concerns)
  
  concerns <- concerns %>%
    filter(str_detect(concerns, "(.*) (.*)", negate = TRUE))
  
  write_csv(concerns, paste0(preprocess_data_dir, "concerns.csv")) }

export_clean_concerns_data(paste0(preprocess_data_dir, "origin-data/keyword_lists/concerns.csv"))