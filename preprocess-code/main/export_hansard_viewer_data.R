# make the stopwords col "ngram"

# figure out count_ngrams -- which might take the place of other code here 

# count_ngrams might belong in word2vec?IDK 
# maybe I need a decade subsetter in R, not just python

# where do i put cache kwic -- currently in word embeddings 


# stop words should be sourced by function 

# this is the export name: hansard_tokens_c19_improved_speaker_names_app_data.csv
# hansard_tokens_decades_1800.csv

library(dplyr)
library(data.table)
library(lubridate)
library(text2vec)

spacy_initialize(virtualenv="/hpc/applications/python_environments/spacy")

# set relative paths
getwd()
setwd()

# do git clone here 

root_dir <- "./hansard-viewer/"
preprocess_code_dir <- "preprocess/"
preproces_data_dir <- "preprocess-data-dir/"
app_data_dir <- "app/app-data/"


#concerns 
#hansard-corpus
#intro 
#searchable debate titles
#hansard tokens
#top speakers
#longest debates
#longest speeches
source(paste0(root_dir, preprocess_code_dir, "speech-lengths/speech_lengths.R"))
source(paste0(root_dir, preprocess_code_dir, "word-embeddings/export_word_embeddings.R"))
source(paste0(root_dir, preprocess_code_dir, "speaker-comparison/export_speaker_comparison_data.R")) # come back to this function export_speaker_comparison_count_for_app()
#nation-concerns
#nation-pairs
#nations
#collocates
#network


