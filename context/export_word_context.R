library(tidyverse)
library(text2vec)

#find all tifs in your directory
dir <- "~/hansard_decades_text2vec_subsets"

#get a list of all files with ndvi in the name in your directory
files <- list.files(path = dir, pattern = "hansard_decades_text2vec_", full.names = TRUE)

for (file in files){
  hansard_subset <- read_csv(file)
  
}
