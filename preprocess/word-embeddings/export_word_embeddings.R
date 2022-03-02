#dir <- "/scratch/group/pract-txt-mine/sbuongiorno/hansard_decades_text2vec_subsets"
#target_dir <- "hansard_decades_wordvectors"

#export_word_embeddings(dir, target_dir, stopwords, view_most_similar = FALSE)

source(paste0(preprocess_code_dir, "word-embeddings/export-word-embeddings-functions/export_word_embeddings_functions.R"))

export_word_embeddings(preprocess_data_dir, view_most_similar = FALSE)
