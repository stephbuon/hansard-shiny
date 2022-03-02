library(dplyr)
library(lubridate)
library(data.table)

source(paste0(preprocess_dir, "add_decade_col.R"))
source(paste0(preprocess_dir, "remove_symbols.R"))
source(paste0(preprocess_dir, "null_to_str_values.R"))
source(paste0(preprocess_dir, "standardize_speaker_cols.R"))
source(paste0(preprocess_dir, "subset_hansard_corpus.R"))

hansard <- fread(paste0(preproces_data_dir, "hansard_c19_improved_speaker_names_2.csv"))

hansard <- add_decade_col(hansard)
hansard <- remove_symbols(hansard)
hansard <- null_to_str_values(hansard)
hansard <- standardize_speaker_cols(hansard)

fwrite(hansard, paste0(preproces_data_dir, "hansard_c19_improved_speaker_names_app_data.csv"))

subset_hansard_corpus(hansard, preprocess_data_dir)