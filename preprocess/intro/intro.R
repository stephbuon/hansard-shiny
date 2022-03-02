intro_viz <- function(root_dir, app_data_dir, hansard) {
  
  hansard <- hansard %>%
    select(debate_id, decade) %>%
    distinct(debate_id, decade) %>%
    group_by(decade) %>%
    summarize(no_of_debates = n())
  
  return(hansard) }

library(data.table)
library(dplyr)

source("intro_functions.R")

hansard <- fread(paste0(preprocess_data_dir, "hansard_c19_improved_speaker_names_app_data.csv"))

hansard <- intro_viz(hansard)

fwrite(hansard, paste0(root_dir, app_data_dir, "number_of_debates_from_1803_1910.csv"))