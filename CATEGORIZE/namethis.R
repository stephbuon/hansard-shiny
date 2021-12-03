library(tidytext)
library(tidyverse)
library(Hmisc)
library(foreach)
library(doParallel)
library(itertools)
library(data.table)

j = 1 
cores=35
cl <- makeCluster(35, outfile = "") 

registerDoParallel(cl)

hansard <- fread("/scratch/group/pract-txt-mine/sbuongiorno/hansard_c19_improved_speaker_names_2.csv")
  
hansard <- hansard %>%
  mutate(decade = year - year %% 10)

decades <- c(1800, 1810, 1820, 1830, 1840, 1850, 1860, 1870, 1880, 1890, 1900, 1910)

for(d in decades) {
  
  
  filtered_hansard <- hansard %>%
    filter(decade == d)
  
  tidy_hansard <- foreach(m = isplitRows(filtered_hansard, chunks=35), .combine='rbind',
                          .packages='tidytext') %dopar% {
                            unnest_tokens(m, ngrams, text, token = "ngrams", n = j) }

dir <- "/scratch/group/pract-txt-mine/sbuongiorno/tokenized_hansard_w_metadata/"
fwrite(tidy_hansard, paste0(dir, "tokenized_hansard_w_metadata", d, ".csv")) }

