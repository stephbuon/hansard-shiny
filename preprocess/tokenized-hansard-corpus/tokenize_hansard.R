library(tidytext)
library(tidyverse)
library(Hmisc)
library(foreach)
library(doParallel)
library(itertools)

tokenize_hansard <- function(hansard) {
  
  hansard <- read_csv(hansard) %>%
    select(sentence_id, speechdate, debate, text, speaker, new_speaker)
  
  unit_length = 1 
  
  cores = 35 
  cl <- makeCluster(35, outfile = "") # outfile as empty on M2 prevents doparallel from redirecting the output?
  registerDoParallel(cl)
  
  hansard <- foreach(m = isplitRows(hansard, chunks=35), .combine='rbind',
                     .packages='tidytext') %dopar% {
                       unnest_tokens(m, ngram, text, token = "ngram", n = unit_length) }
  
  hansard <- hansard %>%
    select(ngram, year)
  
  # I could sort and then do this if I wanted to put these in chronological order
  # At the moment, I don't think all of Hansard is in chronological order
  # hansard$ngram_order <- seq.int(nrow(hansard))
  
  #write_csv(tidy_hansard, file.path(dir, "tokenized_hansard_improved_speaker_names.csv")) 
  write_csv(hansard, "/scratch/group/pract-txt-mine/sbuongiorno/clean_tokenized_hansard.csv") }


tokenize_hansard("/scratch/group/pract-txt-mine/the_new_data_set.csv")
