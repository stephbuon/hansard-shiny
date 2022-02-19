library(tidytext)
library(tidyverse)
library(Hmisc)
library(foreach)
library(doParallel)
library(itertools)

tokenize_hansard <- function(hansard) {
  
  hansard <- fread(hansard) %>%
    select(sentence_id, speaker, disambig_speaker, ngram, year, decade, speech_id, debate_id)
  
  ngram_size = 1 
  
  cores = 35 
  cl <- makeCluster(35, outfile = "") # outfile as empty on M2 prevents doparallel from redirecting the output?
  registerDoParallel(cl)
  
  hansard <- foreach(m = isplitRows(hansard, chunks=35), .combine='rbind',
                     .packages='tidytext') %dopar% {
                       unnest_tokens(m, ngram, text, token = "ngram", n = ngram_size) }
  
  
  # I could sort and then do this if I wanted to put these in chronological order
  # At the moment, I don't think all of Hansard is in chronological order
  # hansard$ngram_order <- seq.int(nrow(hansard)) 
  
  }
