library(tidyverse)
library(lubridate)

export_top_speakers <- function(tidyhans) {
  
  tidy_hans <- read_csv(tidyhans) %>%
    filter(!str_detect(new_speaker, "^NA$")) %>%
    select(-speechdate)
  
  words_per_day <- tidy_hans %>%
    group_by(speechdate, speaker) %>%
    summarize(words_per_day = n())
  
  words_per_day <- words_per_day %>%
    mutate(sp_ranking = rank(desc(words_per_day))) %>%
    group_by(year) %>%
    mutate(sp_ranking_year = rank(desc(words_per_day))) #%>%
#   mutate(max_words = max(words_per_day)) %>%
#   ungroup() %>%
#   mutate(top_speaker = if_else(sp_ranking_year <= b, TRUE, FALSE))
# 
# top_speakers <- words_per_day %>%
#   filter(top_speaker == TRUE)
  
  top_speakers <- words_per_day %>%
    filter(sp_ranking_decade <= b)

  write_csv(top_speakers, "top_speakers_by_year.csv")
  
  words_per_day <- tidy_hans %>%
    group_by(decade, new_speaker) %>%
    summarize(words_per_day = n())
  
  words_per_day <- words_per_day %>%
    group_by(decade) %>%
    mutate(sp_ranking_decade = rank(desc(words_per_day))) #%>%
#   ungroup() %>%
#   mutate(top_speaker = if_else(sp_ranking_decade <= b, TRUE, FALSE))
# 
# top_speakers <- words_per_day %>%
#   filter(top_speaker == TRUE)

  top_speakers <- words_per_day %>%
    filter(sp_ranking_decade <= 5) 
  
  write_csv(top_speakers, "/scratch/group/pract-txt-mine/sbuongiorno/top_speakers_by_decade_improved_speaker_names.csv") 
  
  return(top_speakers) }
