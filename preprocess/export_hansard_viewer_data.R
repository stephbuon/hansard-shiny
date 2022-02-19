# at this point I have added a decade col (make sure there is already a year col )
# I have removed bad symbols from debate and new speaker -- I need to change new speaker to disambig_speaker
# and I have standardized the speaker cols (lower case, no missing periods after mr.)
# this is the export name: hansard_c19_improved_speaker_names_app_data.csv

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
preproces_data_dir <- "preprocess_dir/"
app_data_dir <- "app/app-data/"



source(paste0(root_dir, preprocess_code_dir, "stopwords/stopwords_functions.R"))


#####source(paste0(root_dir, preprocess_code_dir, "hansard-corpus/export_hansard_corpus.R"))
source(paste0(root_dir, preprocess_code_dir, "introduction/export_intro_debates_count.R"))
source(paste0(root_dir, preprocess_code_dir, "searchable-debate-titles/export_searchable_debate_titles.R"))
source(paste0(root_dir, preprocess_code_dir,  "hansard-tokens/export_hansard_tokens.R")) 
# this is the export name: hansard_tokens_c19_improved_speaker_names_app_data.csv
# hansard_tokens_decades_1800.csv
source(paste0(root_dir, preprocess_code_dir,  "top-speakers/export_top_speakers_data.R"))
source(paste0(root_dir, preprocess_code_dir, "longest-debates/export_longest_debates_data.R"))
source(paste0(root_dir, preprocess_code_dir,  "longest-speeches/export_longest_speeches.R"))






source(paste0(code_dir, "speech_lengths.R")) # dep a
source(paste0(code_dir, "speech_lenghts_overview.R")) # dep 



source(paste0(code_dir, "export_hansard_decate_subsets_for_kwic.R"))  
# this will subset full debate text
# then I need to do tokens for word embeddings
# i think i just need cols: ngram, year (no ngrams order)


# need to add decade subsetter, then I can do word 2 vec 
# for text 2 vec just select these: select(year, ngram) %>%
source(paste0(code_dir, "word-embeddings/export_word_embeddings.R")) # also relies on clean hansard corpus

# where do I put more cat 1? hereish? or below more to categorize? 
source() # more_to_categorize.R
source(paste0(code_dir, "clean_speaker_comparison.R"))

source(paste0(code_dir, "export_single_word_concerns.R"))

source(paste0(code_dir, "export_nations.R"))
source(paste0(code_dir, "replace_base_nation_names.R"))
#count_individual_nations

# i gotta do nation_concern_count_py here ish 
# figure out nations-concerns data descrepency 
source(paste0(code_dir, "export_nation_concerns_count.R"))
source(paste0(code_dir, "join_nations_and_geography.R"))
source(paste0(code_dir, "export_treemap_data.R"))


# do all collocates from dem lab
# speaker_adj_noun_collocates.py
source(paste0(code_dir, "clean_collocates.R"))

# I think i have an extract nation pairs script to put here --- python 
source(paste0(code_dir, "clean_nation_pairs_in_debate_titles.R"))
source(paste0(code_dir, "count_nation_pairs_in_debate_titles.R"))


# figure out count_ngrams -- which might take the place of other code here 

# count_ngrams might belong in word2vec?IDK 
# maybe I need a decade subsetter in R, not just python

# where do i put cache kwic -- currently in word embeddings 

#I'd like to put the arguments on this side -- or not?

# at the moment I do not use speaker_stats.R 