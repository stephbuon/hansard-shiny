library(tidyverse)

code_dir <- "/home/stephbuon/projects/hansard-shiny/preprocess/debates/searchable-debate-titles-functions/"
source(paste0(code_dir, "export_unique_debate_titles.R"))
source(paste0(code_dir, "export_searchable_debate_titles_data"))

data_dir <- "~/projects/hansard-shiny/origin-data/"
hansard <- read_csv(paste0(data_dir, "hansard_justnine_w_year.csv"))

export_dir <- paste0(data_dir, "searchable_debate_titles_data/")
dir.create(file.path(export_dir))

debate_titles <- extract_unique_debate_titles(hansard)
debate_titles <- export_searchable_debate_titles_data(debate_titles, export_dir, visualize = FALSE)

