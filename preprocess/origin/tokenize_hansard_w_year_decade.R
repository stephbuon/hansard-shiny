library(tidytext)
library(tidyverse)
library(viridis)
library(Hmisc)
library(foreach)
library(doParallel)
library(itertools)

hansard <- read_csv("/scratch/group/pract-txt-mine/hansard_c19_improved_speaker_names.csv")

hansard <- hansard %>%
select(sentence_id, speechdate, debate, text, speaker, new_speaker)

hansard <- hansard %>%
  mutate(year = year(as.Date(hansard$speechdate)))

decade <- 10

hansard <- hansard %>%
  mutate(decade = year - year %% decade)

hansard <- hansard %>%
select(-speechdate)

j = 1 # how many words constitute a phrase?


#setup parallel backend to use many processors
cores=35#
cl <- makeCluster(35, outfile = "") #not to overload your computer
# outfile as empty prevents doparallel from redirecting the outputs
registerDoParallel(cl)
#registerDoParallel(cores=(Sys.getenv("SLURM_NTASKS_PER_NODE")))

tidy_hansard <- foreach(m = isplitRows(hansard, chunks=35), .combine='rbind',
                       .packages='tidytext') %dopar% {
                         unnest_tokens(m, ngrams, text, token = "ngrams", n = j)
                       }


dir <- setwd("/scratch/group/pract-txt-mine/sbuongiorno")
write_csv(tidy_hansard, file.path(dir, "tokenized_hansard_improved_speaker_names_w_decade.csv"))


#test <- sample_n(tidy_hansard, 10)
