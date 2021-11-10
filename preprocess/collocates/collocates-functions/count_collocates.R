library(tidyverse)

count_collocates <- function(collocates) {

decade <- 10

collocates <- collocates %>%
  mutate(decade = year - year %% decade)

collocates <- collocates %>%
  count(grammatical_collocates, afinn, textblob, vader, decade)

return(collocates)}