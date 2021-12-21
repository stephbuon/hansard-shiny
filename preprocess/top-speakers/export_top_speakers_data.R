library(tidyverse)

code_dir <- "/home/stephbuon/projects/hansard-shiny/preprocess/top-speakers/export-top-speakers-data-functions/"
source(paste0(code_dir, "top_speakers.R"))
source(paste0(code_dir, "export_speaker_favorite_words.R"))

data_dir <- "/scratch/group/pract-txt-mine/"
tokenized_hansard <- read_csv(paste0(data_dir, "tokenized_hansard.csv"))
stopwords <- read_csv(paste0(data_dir, "stopwords.csv"))

export_dir <- paste0(data_dir, "top_speakers_data/")
dir.create(file.path(export_dir))

top_speakers <- export_top_speakers(tokenized_hansard, export_dir)
export_top_speakers_favorite_words(tokenized_hansard, stopwords, top_speakers, export_dir)
