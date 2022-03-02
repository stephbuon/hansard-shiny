library(tidyverse)

code_dir <- "~/projects/hansard-shiny/preprocess/collocates/collocates-functions/"
source(paste0(code_dir, "clean_collocates.R"))
source(paste0(code_dir, "count_collocates.R"))
source(paste0(code_dir, "score_collocates.R"))
source(paste0(code_dir, "reorder_collocates.R"))

#keyword <- "all"
#keyword <- "concerns"
keyword <- "property"

data_dir <- "~/projects/hansard-shiny/origin-data/adj-noun-collocates/"
collocates <- read_csv(paste0(data_dir, keyword, "_adj_noun_collocates.csv"))

collocates <- clean_collocates(collocates, keyword)
collocates <- count_collocates(collocates)

if (keyword == "speakers") {
  collocates <- searchable_speaker(collocates) }

collocates <- score_collocates(collocates, keyword)
collocates <- reorder_collocates(collocates)

write_csv(collocates, paste0("~/projects/hansard-shiny/app/app-data/collocates/clean_", keyword, "_adj_noun_collocates.csv"))
