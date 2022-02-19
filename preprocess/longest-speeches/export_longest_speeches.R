source(paste0(preprocess_code_dir, "/longest-speeches/longest-speeches-functions/subset_hansard_tokens.R"))

hansard <- fread(paste0(preproces_data_dir, "hansard_tokens_c19_improved_speaker_names_app_data.csv"))

longest_speeches <- export_longest_speeches(hansard)

fwrite(longest_speeches, "longest_speeches.csv")


