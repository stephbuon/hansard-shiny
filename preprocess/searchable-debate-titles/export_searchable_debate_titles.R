library(dplyr)

source(paste0(root_dir, preprocess_code_dir, "searchable-debate-titles/export_unique_debate_titles.R"))
source(paste0(root_dir, preprocess_code_dir, "searchable-debate-titles/export_searchable_debate_titles_data"))

hansard <- fread(paste0(preprocess_data_dir, "hansard_c19_improved_speaker_names_app_data.csv"))

hansard <- extract_unique_debate_titles(hansard)

# export_searchable_debate_titles cranks out a lot of data, so I export inside the script instead
# of returning a value
export_dir <- paste0(root_dir, app_data_dir)
hansard <- export_searchable_debate_titles(hansard, export_dir, visualize = FALSE) 
