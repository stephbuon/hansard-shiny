

server <- FALSE

if (server == TRUE) {
  data_dir <- "./app-data/"
} else {
  data_dir <- "~/projects/hansard-shiny/app/app-data/" }



cache_corpus_obj <- function(df) {
  return(corpus(df)) }

cache <- function(decade) {
  hansard <- fread(paste0(data_dir, "kwic/hansard_", decade, ".csv"))
  
  cache_dir_corpus <- cachem::cache_disk(paste0(data_dir, "cache/.rcache_corpus_", decade))
  
  memo_cache_corpus_obj <- memoise(cache_corpus_obj, cache = cache_dir_corpus)
  
  cached_hansard <- memo_cache_corpus_obj(hansard)
  
  return(cached_hansard) }




