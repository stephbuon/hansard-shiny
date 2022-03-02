library(dplyr)
library(tidytext)

source(paste0(preprocess_code_dir, "hansard-tokens/hansard-tokens-functions/hansard_tokens.R"))
source(paste0(preprocess_code_dir, "hansard-tokens/hansard-tokens-functions/count_hansard_tokens.R"))
source(paste0(preprocess_code_dir, "hansard-tokens/hansard-tokens-functions/clean_hansard_tokens.R"))
source(paste0(preprocess_code_dir, "hansard-tokens/hansard-tokens-functions/subset_hansard_tokens.R"))

hansard <- fread(paste0(preproces_data_dir, "hansard_c19_improved_speaker_names_2.csv"))

hansard <- hansard_tokens(hansard)
hansard <- count_hansard_tokens(hansard)
hansard <- clean_hansard_tokens(hansard)

fwrite(hansard, paste(preprocess_data_dir, "hansard_tokens_c19_improved_speaker_names_app_data.csv"))

subset_hansard_tokens(hansard, preproces_data_dir)


