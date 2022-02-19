source(paste0(preprocess_dir, ""))

#/home/stephbuon/projects/hansard-shiny/preprocess/hansard-subsets/hansard-subsets-functions

hansard <- fread(root_dir, preproces_data_dir, "hansard_c19_improved_speaker_names_app_data.csv")

export_dir <- paste0(app_data_dir, "kwic/" )
kwic_decade_subsets(hansard, export_dir)

hansard <- fread(root_dir, preproces_data_dir, "hansard_c19_improved_speaker_names_tokenized_app_data.csv")

export_dir <- paste0()
text2vec_decade_subsets(hansard, export_dir)
