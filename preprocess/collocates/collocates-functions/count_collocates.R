library(tidyverse)

count_collocates <- function(collocates) {

decade <- 10

collocates <- collocates %>%
  mutate(decade = year - year %% decade)


if (keyword == "speakers") { 
  decade <- 10
  
  collocates <- collocates %>%
    mutate(decade = year - year %% decade)
    
  collocates <- collocates %>%
    count(new_speaker, grammatical_collocates, afinn, textblob, vader, decade)} 
else {
  collocates <- collocates %>%
    count(grammatical_collocates, afinn, textblob, vader, decade) }

return(collocates)}