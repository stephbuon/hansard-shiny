library(quanteda)
library(memoise)
library(data.table)
library(text2vec)

data_dir <- "~/projects/hansard-shiny/app/app-data/"

# is this 2 G? 
#cm <- cachem::cache_mem(max_size = 2000 * 1024^2)

laptop <- TRUE
M2 <- FALSE

# set cache directories 

if (laptop == TRUE) {
  cache_dir_corpus_1800 <- cachem::cache_disk(paste0(data_dir, "cache/.rcache_corpus_1800"))
  cache_dir_quanteda_kwic <- cachem::cache_disk(paste0(data_dir, "cache/.rcache_quanteda_kwic_1800")) }
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





hansard_1800 <- fread(paste0(data_dir, "kwic/hansard_1800.csv"))
cached_hansard_1800 <- memo_cache_corpus_obj_1800(hansard_1800)

# hansard_1800 <- fread("/scratch/group/pract-txt-mine/sbuongiorno/hansard_decades/hansard_1800.csv")
# corpus(hansard_1800)



set_window_size <- function(df, window_size){
  
  if (window_size != "Full") {
    df$pre <- str_extract(df$pre, paste0("([^\\s]+\\s+){0,", window_size, "}")) # fix this one
    df$post <- str_extract(df$post, paste0("([^\\s]+\\s+){0,", window_size, "}")) }
  else {
    df$pre <- str_extract(df$pre, paste0("([^\\s]+\\s+){0,300}")) # fix this one
    df$post <- str_extract(df$post, paste0("([^\\s]+\\s+){0,300}")) }
  
  return(df) }