library(tidyverse)
library(tidytext)

hansard <- read_csv("~/hansard_justnine_w_year.csv") %>%
  select(speaker, text, year)

stopwords <- read_csv("~/stopwords.csv") %>%
  rename(word = stop_word)

speaker_names <- read_csv("/users/sbuongiorno/top_speakers.csv")

speaker_names$speaker <- str_replace_all(speaker_names$speaker, "\\*", "")
speaker_names$speaker <- str_replace_all(speaker_names$speaker, "\\[", "")
speaker_names$speaker <- str_replace_all(speaker_names$speaker, "\\]", "")
speaker_names$speaker <- str_replace_all(speaker_names$speaker, "\\(", "")
speaker_names$speaker <- str_replace_all(speaker_names$speaker, "\\)", "")
speaker_names$speaker <- str_replace_all(speaker_names$speaker, "\\.$", "")
speaker_names$speaker <- str_replace(speaker_names$speaker, ",", "")
speaker_names$speaker <- tolower(speaker_names$speaker)

speaker_names_list <- speaker_names$speaker


decade <- 10

hansard <- hansard %>%
  mutate(decade = year - year %% decade)

speaker_names <- speaker_names %>%
  mutate(decade = year - year %% decade)

output <- data.frame()

decades <- c(1800, 1810, 1820, 1830, 1840, 1850, 1860, 1870, 1880, 1890, 1900, 1910)

for(d in 1:length(decades)) {
  
  fdecade <- decades[d]
  
  for(i in 1:length(speaker_names_list)) {
    
    name <- speaker_names_list[i]
    
    print(paste0("Working on ", name))
    
    filtered_speaker_name <- hansard %>%
      filter(str_detect(speaker, regex(name, ignore_case = TRUE)))
    
    print(filtered_speaker_name)
    
    filtered_speaker_name <- filtered_speaker_name %>%
      filter(decade == fdecade)
    
    filtered_speaker_name$speaker <- name # to make names consistent
    
    filtered_speaker_name <- filtered_speaker_name %>%
      unnest_tokens(word, text)
    
    filtered_speaker_name <- filtered_speaker_name %>%
      anti_join(stopwords)
    
    filtered_speaker_name <- filtered_speaker_name %>%
      count(speaker, word, decade) %>%
      arrange(desc(n)) %>%
      slice(1:20)
    
    print(filtered_speaker_name)
    
    output <- bind_rows(output, filtered_speaker_name) } 
}

output <- unique(output) # bc speaker can be mentioned more than once

write_csv(output, "~/speaker_favorite_words_by_decade.csv")

