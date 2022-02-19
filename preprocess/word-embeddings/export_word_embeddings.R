
source("/home/stephbuon/projects/hansard-shiny/preprocess/global_functions.R")

dir <- "/scratch/group/pract-txt-mine/sbuongiorno/hansard_decades_text2vec_subsets"
target_dir <- "hansard_decades_wordvectors"

export_word_embeddings(dir, target_dir, stopwords, view_most_similar = FALSE)

