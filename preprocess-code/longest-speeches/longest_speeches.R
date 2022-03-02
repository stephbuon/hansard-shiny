export_longest_speeches <- function(tokenized_hansard) {
  
  tokenized_hansard <- read_csv(tokenized_hansard)
  
  metadata <- tokenized_hansard %>%
    select(speaker, speechdate, speech_id, debate) %>%
    distinct()
  
  words_per_speech <- tokenized_hansard %>%
    group_by(speech_id) %>%
    summarize(count_per_speech = n()) %>%
    ungroup()
  
  words_per_speech <- words_per_speech %>% 
    inner_join(metadata, on = "speech_id") %>%
    mutate(year = year(speechdate)) %>%
    mutate(sp_ranking = rank(desc(count_per_speech))) %>%
    group_by(year(speechdate)) %>%
    mutate(sp_ranking_yr = rank(desc(count_per_speech))) %>%
    ungroup()
  
  longest_speech_decade <- words_per_speech %>%
    group_by(10*floor(year/10)) %>%
    mutate(sp_ranking_decade = rank(desc(count_per_speech))) %>%
    filter(sp_ranking_decade < 5) %>%
    ungroup() %>%
    inner_join(metadata) 
  
  fwrite(longest_speech_decade, "longest_speeches.csv") }


hansard <- fread(paste0(preproces_data_dir, "hansard_tokens_c19_improved_speaker_names_app_data.csv"))

export_longest_speeches(hansard)
