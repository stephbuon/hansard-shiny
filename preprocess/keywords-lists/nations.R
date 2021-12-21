library(tidyverse)

export_clean_nations_data <- function(nation) {
  
  nations <- read_csv(nations)
  
  nations <- unique(nations)
  
  nations <- nations %>%
    filter(!str_detect(nations, "\\'s'")) # get rid of possessive nations
  
  write_csv(nations, "~/nations.csv") }

export_clean_nations_data("~/collaborative_nations.csv")
