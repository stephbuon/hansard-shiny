library(tidyverse)
library(text2vec)
#library(MASS)

clean_data_for_word_embeddings <- function(data_decade_subset) {
  
  data_decade_subset <- data_decade_subset %>%
    filter(!str_detect(ngrams, "[[:digit:]]"))
  
  data_decade_subset$ngrams <- str_replace(data_decade_subset$ngrams, "'s", "")
  
  stopwords <- import_stopwords_as_regex()
  
  data_decade_subset <- data_decade_subset %>%
    filter(!str_detect(ngrams, stopwords))
  
  return(data_decade_subset) }


view_most_similar_words <- function(word_vectors, keyword, n_view) {
  kw = word_vectors[keyword, , drop = F]
  
  cos_sim_rom = sim2(x = word_vectors, y = kw, method = "cosine", norm = "l2")
  
  print(head(sort(cos_sim_rom[,1], decreasing = TRUE), n_view)) }


export_word_embeddings <- function(dir, target_dir, stopwords, view_most_similar) {
  
  files <- list.files(path = dir, pattern = "hansard_decades_text2vec", full.names = TRUE)
  
  for (file in files) {
    data_decade_subset <- read_csv(file)
    
    data_decade_subset <- clean_data_for_word_embeddings(data_decade_subset, stopwords)
    
    first_year_label <- first(data_decade_subset$year)
    last_year_label <- last(data_decade_subset$year)
    
    vocab_list = list(data_decade_subset$ngrams)
    
    it = itoken(vocab_list, progressbar = FALSE)
    vocab = create_vocabulary(it)
    
    # term_count_min is the minimum number of times a word is stated
    vocab = prune_vocabulary(vocab, term_count_min = 40)
    
    # just exploring words and their counts to understand the data better
    # temp <- hansard_decade_subset %>%
    #    count(ngrams)
    
    vectorizer = vocab_vectorizer(vocab)
    
    # The default suggestion was a window of 10, but I am using a window of 5 because the results seem better
    # for the Hansard data set -- I get less stop words in results
    tcm = create_tcm(it, vectorizer, skip_grams_window = 5)
    
    glove = GlobalVectors$new(rank = 4, x_max = 100)
    
    wv_main = glove$fit_transform(tcm, n_iter = 1000, convergence_tol = 0.00000001, n_threads = 24)
    
    wv_context = glove$components
    
    # The developers of the method suggest that sum/mean may work best when creating a matrix
    print("Finding sum/mean")
    word_vectors = wv_main + t(wv_context)
    
    if (view_most_similar == TRUE) { # test to see if this works 
      view_most_similar_wrods(word_vectors, keyword, 40) }
    
    print(paste0("Checking if ", dir, "/", target_dir, " exists."))
    if(!dir.exists(file.path(dir, target_dir))) {
      print(paste0("Creating ", dir, target_dir)) }
    
    ifelse(!dir.exists(file.path(dir, target_dir)), dir.create(file.path(dir, target_dir)), FALSE)
    
    fname <- paste0(dir, "/", target_dir, "/", "hansard_word_vectors_", first_year_label, "_", last_year_label, ".txt")
    
    print("Writing word embeddings to disk")
    #write.matrix(hansard_word_vectors, file = fname, sep = ",")
    write.table(word_vectors, file = fname)
    
  }
  
  
}
