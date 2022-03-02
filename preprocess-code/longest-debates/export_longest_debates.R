export_longest_debates <- function(tokenized_hansard) {
  
  metadata <- tokenized_hansard %>%
    select(speaker, speechdate, speech_id, debate) %>%
    distinct()
  
  debate_length <- tokenized_hansard %>%
    group_by(speechdate, debate_id, debate) %>% # changed this from debate
    summarize(words_per_debate = n()) %>%
    ungroup() # %>%
    # mutate(year = year(speechdate)) %>%
    # mutate(decade = 10*floor(year/10))
  
  debate_length <- debate_length %>% 
    group_by(decade) %>%
    mutate(decade_ranking = rank(desc(words_per_debate))) %>%
    ungroup() %>%
    group_by(year) %>%
    mutate(year_ranking = rank(desc(words_per_debate))) %>%
    ungroup() %>%
    inner_join(metadata) 
  
  longest_debates_in_decade <- debate_length %>%
    filter(decade_ranking <= 5) %>%
    inner_join(metadata) %>%
    select(-speaker, -speech_id) %>%
    distinct() 
  
  write_csv(longest_debates_in_decade, "longest_debates.csv") 
  
  return(longest_debates_in_decade) }


hansard_tokens <- fread(paste0(preprocess_data_dir, "hansard_tokens_c19_improved_speaker_names_app_data.csv"))

longest_debates <- export_longest_debates(hansard_tokens)

fwrite(longest_debates, paste0(preprocess_data_dir, "longest_debates.csv"))
