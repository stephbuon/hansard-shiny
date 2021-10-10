

load('~/Downloads/shakes_words_df_4text2vec.RData')

library(text2vec)

shakes_words_ls = list(shakes_words$word)

it = itoken(shakes_words_ls, progressbar = FALSE)
shakes_vocab = create_vocabulary(it)
shakes_vocab = prune_vocabulary(shakes_vocab, term_count_min = 5)

# maps words to indices
vectorizer = vocab_vectorizer(shakes_vocab)

# use window of 10 for context words
shakes_tcm = create_tcm(it, vectorizer, skip_grams_window = 10)



#glove = GlobalVectors$new(word_vectors_size = 50, vocabulary = shakes_vocab, x_max = 10)

glove = GlobalVectors$new(rank = 50, x_max = 10)

#wv_main = glove$fit_transform(tcm, n_iter = 10, convergence_tol = 0.01, n_threads = 8)

shakes_wv_main = glove$fit_transform(shakes_tcm, n_iter = 1000, convergence_tol = 0.00001)

# dim(shakes_wv_main)

shakes_wv_context = glove$components

# dim(shakes_wv_context)

# Either word-vectors matrices could work, but the developers of the technique
# suggest the sum/mean may work better
shakes_word_vectors = shakes_wv_main + t(shakes_wv_context)



rom = shakes_word_vectors["romeo", , drop = F]
# ham = shakes_word_vectors["hamlet", , drop = F]

cos_sim_rom = sim2(x = shakes_word_vectors, y = rom, method = "cosine", norm = "l2")
# head(sort(cos_sim_rom[,1], decreasing = T), 10)



love = shakes_word_vectors["love", , drop = F]

cos_sim_rom = sim2(x = shakes_word_vectors, y = love, method = "cosine", norm = "l2")

head(sort(cos_sim_rom[,1], decreasing = T), 10)


test = shakes_word_vectors["romeo", , drop = F] -
  shakes_word_vectors["mercutio", , drop = F] +
  shakes_word_vectors["nurse", , drop = F]

cos_sim_test = sim2(x = shakes_word_vectors, y = test, method = "cosine", norm = "l2")

test = shakes_word_vectors["romeo", , drop = F] - 
  shakes_word_vectors["juliet", , drop = F] + 
  shakes_word_vectors["cleopatra", , drop = F] 

cos_sim_test = sim2(x = shakes_word_vectors, y = test, method = "cosine", norm = "l2")