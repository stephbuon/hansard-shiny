top_speakers <- function(tidyhans) {
  
  tidy_hans <- fread(tidyhans) %>%
    select(sentence_id, disambig_speaker, year, decade, speechdate, ngram) %>%
    filter(!str_detect(disambig_speaker, "^NA$")) 
  
  words_per_day <- tidy_hans %>%
    group_by(speechdate, disambig_speaker) %>%
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

  write_csv(top_speakers, "top_speakers_by_year.csv") # why? 
  
  words_per_day <- tidy_hans %>%
    group_by(decade, disambig_speaker) %>%
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
  
  return(top_speakers) }
