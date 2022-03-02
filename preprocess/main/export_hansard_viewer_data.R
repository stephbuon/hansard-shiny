# at this point I have added a decade col (make sure there is already a year col )
# I have removed bad symbols from debate and new speaker -- I need to change new speaker to disambig_speaker
# and I have standardized the speaker cols (lower case, no missing periods after mr.)
# this is the export name: hansard_c19_improved_speaker_names_app_data.csv

# make the stopwords col "ngram"



# figure out count_ngrams -- which might take the place of other code here 

# count_ngrams might belong in word2vec?IDK 
# maybe I need a decade subsetter in R, not just python

# where do i put cache kwic -- currently in word embeddings 

#I'd like to put the arguments on this side -- or not?

# at the moment I do not use speaker-stats/speaker_stats.R 

# stop words should be sourced by function 

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


# re do stop words (instead of global functions)

# this is the export name: hansard_tokens_c19_improved_speaker_names_app_data.csv
# hansard_tokens_decades_1800.csv


#concerns 
source(paste0(root_dir, preprocess_code_dir, "hansard-corpus/export_hansard_corpus.R")) # add export by decade here : hansard_decade_1800.csv -- just sentence_id and text
#intro 
#searchable debate titles
#hansard tokens
source(paste0(root_dir, preprocess_code_dir, "top-speakers/export_top_speakers_data.R"))
source(paste0(root_dir, preprocess_code_dir, "longest-debates/export_longest_debates_data.R"))
source(paste0(root_dir, preprocess_code_dir, "longest-speeches/export_longest_speeches.R"))
source(paste0(root_dir, preprocess_code_dir, "speech-lengths/speech_lengths.R"))
source(paste0(root_dir, preprocess_code_dir, "word-embeddings/export_word_embeddings.R"))
source(paste0(root_dir, preprocess_code_dir, "speaker-comparison/export_speaker_comparison_data.R")) # come back to this function export_speaker_comparison_count_for_app()
#nation-concerns
#nation-pairs
#nations
#collocates
#network


