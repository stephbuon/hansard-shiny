`%like%` <- function (x, pattern) { 
  stringi::stri_detect_regex(x, pattern, case_insensitive=TRUE)
}

#nvm -- this has to be in the UI
#ns <- NS(id)


import_stopwords_as_regex <- function() {
  
  stopwords <- read_csv("/scratch/group/pract-txt-mine/sbuongiorno/stopwords_text2vec.csv") 
  
  stopwords <- stopwords %>%
    summarise(all = paste0(stop_word, collapse="|"))
  
  stopwords <- stopwords$all 
  
  return(stopwords) }
