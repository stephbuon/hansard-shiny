source(paste0(preprocess_code_dir, "/speech-lengths/speech-lengths-functions/find_speech_length.R"))

hansard <- fread(paste0(preproces_data_dir, "hansard_tokens_c19_improved_speaker_names_app_data.csv"))

speech_lengths <- find_speech_length(hansard)
export_speech_lengths_overview(preprocess_data_dir, speech_lengths)
export_speech_length_type_data(preprocess_data_dir)

fwrite(longest_speeches, "longest_speeches.csv")


