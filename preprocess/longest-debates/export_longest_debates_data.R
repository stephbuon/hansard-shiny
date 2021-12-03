library(tidyverse)

code_dir <- "/home/stephbuon/projects/hansard-shiny/preprocess/longest-debates/export-longest-debate-data-functions/"
source(paste0(code_dir, "export_longest_debates.R"))
source(paste0(code_dir, "export_wordcloud_data.R"))

data_dir <- "/scratch/group/pract-txt-mine/"
tokenized_hansard <- read_csv(paste0(data_dir, "tokenized_hansard.csv"))
stopwords <- read_csv(paste0(data_dir, "stopwords.csv"))

export_dir <- paste0(data_dir, "longest_debates_data/")
dir.create(file.path(export_dir))

longest_debates <- export_longest_debates(tokenized_hansard, export_dir)
export_wordcloud_data(tokenized_hansard, stopwords, longest_debates, export_dir)






