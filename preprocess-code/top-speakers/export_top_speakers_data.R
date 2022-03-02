source(paste0(preprocess_code_dir, "top-speakers/top-speakers-functions/top_speakers.R"))
source(paste0(preprocess_code_dir, "top-speakers/top-speakers-functions/export_speaker_favorite_words.R"))

hansard_tokens <- fread(paste0(preprocess_data_dir, "hansard_tokens_c19_improved_speaker_names_app_data.csv"))

top_speakers <- top_speakers(hansard_tokens)

rm(hansard_tokens)

fwrite(top_speakers, paste0(preprocess_data_dir, "top_speakers_by_decade.csv"))

speaker_favorite_words <- export_top_speakers_favorite_words(top_speakers, stopwords)

fwrite(speaker_favorite_words, paste0(preprocess_data_dir, "speaker_favorite_words_by_decade.csv"))