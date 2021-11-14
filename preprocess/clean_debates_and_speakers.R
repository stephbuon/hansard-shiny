library(tidyverse)

hansard_speakers <- read_csv("/scratch/group/pract-txt-mine/hansard_c19_improved_speaker_names_2.csv")

hansard <- read_csv("~/hansard_justnine_w_year.csv")

decade <- 10 

hansard <- hansard %>% 
  select(debate_id, debate, year)

hansard <- left_join(hansard, hansard_speakers, by = "sentence_id")

remove <- c("\\[", "\\]", "\\(", "\\)", "—", "-", "\\.$", "\\*", "\"", "\\,", "\'", "\\>", "^ ")
remove <- paste0(remove, collapse = '|' )

hansard$debate <- str_replace_all(hansard$debate, remove, "")
hansard$new_speaker <- str_replace_all(hansard$new_speaker, remove, "")

hansard$new_speaker <- str_replace(hansard$new_speaker, "mr ", "mr. ")


# remove speaker column and just have new_speaker -- but also rename

write_csv(hansard, "/scratch/group/pract-txt-mine/sbuongiorno/hansard_c19_improved_speaker_names_app_data.csv")