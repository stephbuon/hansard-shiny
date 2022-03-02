source(paste0(preprocess_code_dir, "speaker-comparison/speaker-comparison-functions/export_speaker_comparison_radio_buttons.R"))
source(paste0(preprocess_code_dir, "speaker-comparison/speaker-comparison-functions/more_to_categorize.R"))

hansard <- fread(paste0(preproces_data_dir, "hansard_tokens_c19_improved_speaker_names_app_data.csv"))

export_speaker_comparison_radio_buttons(hansard)

export_speaker_comparison_count_for_app()
