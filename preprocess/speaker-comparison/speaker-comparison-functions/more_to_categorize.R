library(tidyverse)


export_speaker_comparison_count_for_app <- function() {
  
  top_speakers <- fread(paste0(preprocess_data_dir, "top_speakers_by_decade.csv")) %>%
    select(-sp_ranking_decade, -top_speaker)
  
  tokenized_hansard <- fread(paste0(preprocess_data_dir, "hansard_tokens_c19_improved_speaker_names_app_data"))
  
  out <- left_join(top_speakers, tokenized_hansard, by = c("new_speaker", "decade"))
  
  speaker_count <- out %>%
    count(decade, new_speaker, ngrams)
  
  #fwrite(speaker_count, paste0(app_data_dir, "speaker_comparison_speaker_count.csv"))
  
  
  
  
  stopwords <- read_csv("/scratch/group/pract-txt-mine/sbuongiorno/stopwords_text2vec.csv") %>%
    rename(ngrams = stop_word)
  
  stopwords <- stopwords %>%
    summarise(all = paste0(ngrams, collapse="|"))
  
  stopwords <- stopwords$all
  
  
  
  
  speaker_count_2 <- speaker_count %>%
    filter(!grepl("[[:digit:]]", ngrams))
  
  speaker_count_2$ngrams <- str_replace(speaker_count_2$ngrams, "'s", "")
  
  speaker_count_2 <- speaker_count_2 %>%
    filter(!str_detect(ngrams, stopwords))
  
  
  speaker_count_2 <- speaker_count_2 %>%
    filter(n > 4)
  
  
  #write_csv(speaker_count_2, "~/speaker_comparison_speaker_count_for_app.csv")
  
  
  #h <- read_csv("~/projects/hansard-shiny/speaker_comparison_speaker_count_for_app.csv")
  
  
  h <- speaker_count_2 %>%
    mutate(clean_new_speaker = new_speaker)
  
  h$clean_new_speaker <- str_replace_all(h$clean_new_speaker, "_", " ") 
  h$clean_new_speaker <- str_replace_all(h$clean_new_speaker, "[:digit:]", "")
  
  h$clean_new_speaker <- str_to_title(h$clean_new_speaker)
  
  write_csv(h, "~/projects/hansard-shiny/speaker_comparison_speaker_count_for_app_2.csv")
  
  #t <- h %>%
  #  distinct(decade, new_speaker)
  
  
  
  
  
  
}



