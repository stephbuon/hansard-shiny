source(paste0(preprocess_code_dir, "longest-debates/export-longest-debate-data-functions/export_longest_debates.R"))
source(paste0(preprocess_code_dir, "longest-debates/export-longest-debate-data-functions/export_wordcloud_data.R"))

hansard_tokens <- fread(paste0(preprocess_data_dir, "hansard_tokens_c19_improved_speaker_names_app_data.csv"))

longest_debates <- export_longest_debates(hansard_tokens)

fwrite(longest_debates, paste0(preprocess_data_dir, "longest_debates.csv"))

export_wordcloud_data(hansard_tokens, longest_debates)






