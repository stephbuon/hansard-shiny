library(tidyverse)
library(tidytext)

hansard <- read_csv("~/hansard_justnine_w_year.csv")

stopwords <- read_csv("~/stopwords.csv") %>%
  rename(word = stop_word)

speaker_names <- read_csv("~/projects/hansard-shiny/data/speakers/top_speakers.csv")
speaker_names_list <- speaker_names$speaker

output <- data.frame()

for(i in 1:length(speaker_names_list)) {
    
    name <- speaker_names_list[i]
    
    print(paste0("Working on ", name))
    
    filtered_speaker_name <- hansard %>%
      filter(str_detect(speaker, regex(name, ignore_case = TRUE)))
    
    filtered_speaker_name <- filtered_speaker_name %>%
      unnest_tokens(word, speaker)
    
    filtered_speaker_name <- filtered_speaker_name %>%
      anti_join(stopwords)
    
    filtered_speaker_name <- filtered_speaker_name %>%
      count(word, sort = TRUE) %>%
      slice(100)
    
    output <- bind_rows(output, filtered_speaker_name) } 

write_csv(output, "~/")