library(tidyverse)

code_dir <- "~/projects/hansard-shiny/preprocess/collocates/collocates-functions/"
source(paste0(code_dir, "clean_collocates.R"))
source(paste0(code_dir, "count_collocates.R"))
source(paste0(code_dir, "searchable_speaker.R"))
source(paste0(code_dir, "score_collocates.R"))
source(paste0(code_dir, "reorder_collocates.R"))

keyword <- "speakers"

data_dir <- "~/projects/hansard-shiny/app-data/speakers/speaker_adj_noun_collocates.csv"
collocates <- read_csv(data_dir)

collocates <- clean_collocates(collocates, keyword)
collocates <- count_collocates(collocates)

if (keyword == "speakers") {
  collocates <- searchable_speaker(collocates) }

collocates <- score_collocates(collocates, keyword)
collocates <- reorder_collocates(collocates)

if (keyword == "speakers") { # messsy -- fix later -- instead of having ngrams column in module code, have item?
  collocates <- collocates %>%
    rename(ngrams = grammatical_collocates)
  
  collocates$clean_new_speaker <- str_to_title(collocates$clean_new_speaker) # add this to the seaerchable speakers code
   
  }


write_csv(collocates, paste0("~/projects/hansard-shiny/app-data/speakers/clean_speaker_adj_noun_collocates.csv"))



