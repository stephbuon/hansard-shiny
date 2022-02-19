source("intro_debates_count_functions.R")

hansard <- fread(paste0(preprocess_data_dir, "hansard_c19_improved_speaker_names_app_data.csv"))

hansard <- intro_viz(hansard)

fwrite(hansard, paste0(root_dir, app_data_dir, "number_of_debates_from_1803_1910.csv"))