library(quanteda)
library(memoise)
library(data.table)
library(text2vec)

# is this 2 G? 
#cm <- cachem::cache_mem(max_size = 2000 * 1024^2)

laptop <- FALSE
M2 <- TRUE

# set cache directories 

if (laptop == TRUE) {
  cache_dir_corpus_1800 <- cachem::cache_disk("/home/stephbuon/projects/hansard-shiny/cache/.rcache_corpus_1800")
  cache_dir_quanteda_kwic <- cachem::cache_disk("/home/stephbuon/projects/hansard-shiny/cache/.rcache_quanteda_kwic_1800") }
if (M2 == TRUE) {
  M2_cache_dir <- "/scratch/group/pract-txt-mine/sbuongiorno/cache"
  cache_dir_quanteda_kwic <- cachem::cache_disk(paste0(M2_cache_dir, "/.rcache_quanteda_kwic_1800")) }


# create cache functions 

cache_corpus_obj_1800 <- function(df) {
  return(corpus(df)) }
memo_cache_corpus_obj_1800 <- memoise(cache_corpus_obj_1800, cache = cache_dir_corpus_1800)


quanteda_kwic <- function(df, kw) { # I can do fixed or regex 
  
  if (length(kw == 1)) {
    j <- as.data.table(kwic(df, kw, window = 300, valuetype = "fixed", separator = " ", case_insensitive = TRUE))
    j <- select(j, -docname, -to, -from, -pattern) 
    
    
    return(j) } }
# else {
#   return(kwic(df, pattern = phrase(kw), window = 300, valuetype = "fixed", separator = " ", case_insensitive = TRUE)) } }
memo_quanteda_kwic <- memoise(quanteda_kwic, cache = cache_dir_quanteda_kwic)


#hansard_1800 <- fread("/home/stephbuon/projects/hansard-shiny/app-data/kwic/hansard_1800.csv")
#cached_hansard_1800 <- memo_cache_corpus_obj_1800(hansard_1800)

hansard_1800 <- fread("/scratch/group/pract-txt-mine/sbuongiorno/hansard_decades/hansard_1800.csv")
cached_hansard_1800 <- corpus(hansard_1800)



#source("/home/stephbuon/projects/hansard-shiny/modules/kwic/kwic_functions.R")


cache_kwic <- function() {
  
  decades <- c(1800)#, 1810, 1820, 1830, 1840, 1850, 1860, 1870, 1880, 1890)
  
  for(d in 1:length(decades)) {
    
    #table <- paste0("~/projects/hansard-shiny/app-data/word_embeddings/hansard_decades_wordvectors_10192021/hansard_word_vectors_", d, ".txt")
    table <- paste0("/scratch/group/pract-txt-mine/sbuongiorno/hansard_decades_text2vec_subsets/working_on_for_cache/hansard_word_vectors_", d, ".txt")
    word_vectors <- as.matrix(read.table(table, as.is = TRUE))
    
    vocab <- rownames(word_vectors)
    vocab <- tolower(vocab) # give a vocab list
    
    
    for(item in vocab) {
      print(item)
      
      j <- memo_quanteda_kwic(cached_hansard_1800, item)
      x <- as.character(item)
      write(x, file="~/cached_vocab.txt",append=TRUE) } } }


























































#source("/home/stephbuon/projects/hansard-shiny/modules/kwic/kwic_functions.R")

cache_kwic <- function() {
  
  decades <- c(1800)#, 1810, 1820, 1830, 1840, 1850, 1860, 1870, 1880, 1890)
  
  for(d in 1:length(decades)) {
    
    table <- paste0("~/projects/hansard-shiny/app-data/word_embeddings/hansard_decades_wordvectors_10192021/hansard_word_vectors_", d, ".txt")
    #table <- paste0("/scratch/group/pract-txt-mine/sbuongiorno/hansard_decades_text2vec_subsets/working_on_for_cache/hansard_word_vectors_", d, ".txt")
    word_vectors <- as.matrix(read.table(table, as.is = TRUE))
    
    
    vocab <- rownames(word_vectors)
    vocab <- tolower(vocab) # give a vocab list
    
    if(vocab %in% rownames(word_vectors)) { 
      
      kw = word_vectors[vocab, , drop = F]
      
      cos_sim_rom = sim2(x = word_vectors, y = kw, method = "cosine", norm = "l2")
      
      forplot <- as.data.frame(sort(cos_sim_rom[,1], decreasing = T)[2:16])
      
      colnames(forplot)[1] <- "similarity"
      
      forplot$word <- rownames(forplot)
      
      forplot <- forplot %>%
        mutate(decade = d)
      
      #forplot <- forplot %>%
      #  filter(row_id == s$pointNumber)
      
      word <- forplot$word
      
      searched <- list()
      for (w in word) {
        
        print(w)
        
        if (!w %in% searched) {
          j <- memo_quanteda_kwic(cached_hansard_1800, w) 
          searched <- append(searched, w) 
          
          x <- as.character(w)
          write(x, file="~/cached_vocab.txt",append=TRUE)
          
          }
  
      }

    } 
    
    
    
    
    
    
  }
  
}

