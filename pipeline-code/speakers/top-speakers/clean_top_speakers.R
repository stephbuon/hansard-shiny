library(tidyverse)

speaker_names <- read_csv("~/projects/hansard-shiny/data/speakers/top_speakers.csv")

speaker_names$speaker <- gsub("\\*", "", speaker_names$speaker)
speaker_names$speaker <- str_replace_all(speaker_names$speaker, "\\,", "")
speaker_names$speaker <- str_replace_all(speaker_names$speaker, "\\[", "")
speaker_names$speaker <- str_replace_all(speaker_names$speaker, "\\]", "")
speaker_names$speaker <- str_replace_all(speaker_names$speaker, "\\(", "")
speaker_names$speaker <- str_replace_all(speaker_names$speaker, "\\)", "")
speaker_names$speaker <- str_replace_all(speaker_names$speaker, "\\.$", "")

speaker_names$speaker <- str_replace(speaker_names$speaker, "mr ", "mr. ")


speaker_names$speaker <- tolower(speaker_names$speaker)

write_csv(speaker_names, "~/top_speakers.csv")
