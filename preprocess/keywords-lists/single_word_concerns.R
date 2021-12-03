library(tidyverse)

export_clean_concerns_data <- function(concerns) {
  
  concerns <- read_csv(concerns)
  
  concerns <- unique(concerns)
  
  concerns <- concerns %>%
    filter(str_detect(concerns, "(.*) (.*)", negate = TRUE))
  
  write_csv(concerns, "concerns.csv") }


export_clean_concerns_data("~/concerns.csv")