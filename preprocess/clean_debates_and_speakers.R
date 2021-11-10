library(tidyverse)

hansard <- read_csv("/scratch/group/pract-txt-mine/hansard_c19_improved_speaker_names.csv")

remove <- c("\\[", "\\]", "\\(", "\\)", "—", "-", "\\.", "\\*", "\"")

remove <- paste0(remove, collapse = '|' )

hansard <- str_replace_all(hansard$debate)
hansard <- str_replace_all(hansard$speaker)

write_csv(hansard, "/scratch/group/pract-txt-mine/sbuongiorno/hansard_c19_improved_speaker_names_app_data.csv")