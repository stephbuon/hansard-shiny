library(memoise)
library(cachem)

#assigned in global scope
cache_dir <<- tempdir() 
#cache_size <<- 200 * 1024^3

cache_size <- 8589934592
  
memo_cache_corpus_obj <- memoise::memoise(cache_corpus_obj, cache = cachem::cache_disk(dir = cache_dir, max_size = cache_size))


df <- memo_cache_corpus_obj(b)

g <- memo_cache_corpus_obj(b)


cache.percent.full <- function() {
  used.mem <- file.size(dir(cache.dir, full.names = TRUE)) %>% 
    sum() 
  return(used.mem/cache.size * 100)
}

cache.percent.full()
