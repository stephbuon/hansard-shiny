library(tidyverse)
library(text2vec)
#library(MASS)

stopwords <- read_csv("/scratch/group/pract-txt-mine/sbuongiorno/stopwords_text2vec.csv") %>%
  rename(ngrams = stop_word)

stopwords <- stopwords %>%
  summarise(all = paste0(ngrams, collapse="|"))

stopwords <- stopwords$all

dir <- "/scratch/group/pract-txt-mine/sbuongiorno/hansard_decades_text2vec_subsets"
target_dir <- "hansard_decades_wordvectors"

#get a list of all files with ndvi in the name in your directory
files <- list.files(path = dir, pattern = "hansard_decades_text2vec", full.names = TRUE)

for (file in files){
  hansard_decade_subset <- read_csv(file)
  #hansard_decade_subset <- read_csv("/scratch/group/pract-txt-mine/sbuongiorno/hansard_decades_text2vec_subsets/hansard_decades_text2vec1803_1807.csv")
  
  hansard_decade_subset <- hansard_decade_subset %>%
    filter(!grepl("[[:digit:]]", ngrams))
  
  hansard_decade_subset$ngrams <- str_replace(hansard_decade_subset$ngrams, "'s", "")
  
  hansard_decade_subset <- hansard_decade_subset %>%
    filter(!str_detect(ngrams, stopwords))
  
  #hansard_decade_subset <- hansard_decade_subset %>%
  #  arrange(year) should not have this -- might mess of token order 
  first_year_label <- first(hansard_decade_subset$year)
  last_year_label <- last(hansard_decade_subset$year)
  
  hansard_list = list(hansard_decade_subset$ngrams)
  
  it = itoken(hansard_list, progressbar = FALSE)
  hansard_vocab = create_vocabulary(it)
  
  hansard_vocab = prune_vocabulary(hansard_vocab, term_count_min = 40)
  
  # trash <- hansard_decade_subset %>%
  #    count(ngrams)
  
  
  vectorizer = vocab_vectorizer(hansard_vocab)
  
  # use window of 10 for context words
  hansard_tcm = create_tcm(it, vectorizer, skip_grams_window = 5)
  
  glove = GlobalVectors$new(rank = 4, x_max = 100)
  
  hansard_wv_main = glove$fit_transform(hansard_tcm, n_iter = 1000, convergence_tol = 0.00000001, n_threads = 24)
  
  hansard_wv_context = glove$components
  
  #Either word-vectors matrices could work, but the developers of the technique
  # suggest the sum/mean may work better
  print("Finding sum/mean")
  hansard_word_vectors = hansard_wv_main + t(hansard_wv_context)
  
  # 
  # 
  # kw = hansard_word_vectors["irish", , drop = F]
  # 
  # cos_sim_rom = sim2(x = hansard_word_vectors, y = kw, method = "cosine", norm = "l2")
  # 
  # head(sort(cos_sim_rom[,1], decreasing = T), 60)
  # 
  
  
  #print("Transfering words from index")
  #hansard_word_vectors$word <- rownames(hansard_word_vectors)
  
  #print("Casting matrix to DF")
  #hansard_word_vectors <- as.data.frame(hansard_word_vectors)
  
  
  #print("Checking if dir exists")
  ifelse(!dir.exists(file.path(dir, target_dir)), dir.create(file.path(dir, target_dir)), FALSE)
  
  fname <- paste0(dir, "/", target_dir, "/", "hansard_word_vectors_", first_year_label, "_", last_year_label, ".txt")
  
  #print("Writing word vectors to csv")
  #write_csv(hansard_word_vectors, paste0(dir, "/", target_dir, "/", "hansard_word_vectors_", first_year_label, "_", last_year_label, ".csv"))
  
  print("Writing matrix")
  #write.matrix(hansard_word_vectors, file = fname, sep = ",")
  write.table(hansard_word_vectors, file=fname)#, row.names=F, col.names=F)
  
}



